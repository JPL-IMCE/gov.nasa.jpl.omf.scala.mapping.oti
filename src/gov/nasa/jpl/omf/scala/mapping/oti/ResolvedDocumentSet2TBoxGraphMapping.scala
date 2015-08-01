/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2015, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.mapping.oti

import gov.nasa.jpl.omf.scala.core._
import org.omg.oti._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.canonicalXMI._
import scala.reflect.runtime.universe._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

case class ResolvedDocumentSet2TBoxGraphMapping[Uml <: UML, Omf <: OMF]()(
  implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

case class ResolvedDocumentTBoxGraph[Uml <: UML, Omf <: OMF](
  document: Document[Uml],
  tbox: Omf#ImmutableModelTerminologyGraph,
  aspects: Map[UMLElement[Uml], Omf#ModelEntityAspect],
  concepts: Map[UMLElement[Uml], Omf#ModelEntityConcept] )(
    implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

case class Document2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF](
  resolved: ResolvedDocumentSet[Uml],
  document2tboxGraphs: Map[Document[Uml], Omf#ImmutableModelTerminologyGraph],
  documents2map: Set[Document[Uml]] )(
    implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

object ResolvedDocumentSet2TBoxGraphMapping {

  def resolvedDocumentSet2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF]( resolved: ResolvedDocumentSet[Uml] )(
    implicit umlOps: UMLOps[Uml],
    omfOps: OMFOps[Omf],
    omfStore: Omf#Store,
    catalogIRIMapper: CatalogURIMapper ): Try[Document2TBoxGraphCorrespondences[Uml, Omf]] = {

    val sortedDocuments = resolved.ds.topologicalSort( resolved.g ) match {
      case Left( document )         => List( document )
      case Right( documents ) => documents
    }

    import omfOps._

    val ( document2tboxGraphs, documents2map ) =
      ( ( Map[Document[Uml], Omf#ImmutableModelTerminologyGraph](),
        Set[Document[Uml]]() ) /: sortedDocuments ) {
          case ( ( ( document2tboxMap, d2map ), document ) ) =>
            catalogIRIMapper.resolveURI( document.uri, catalogIRIMapper.loadResolutionStrategy ) match {
              case Failure( t ) =>
                return Failure( t )
              case Success( None ) =>
                System.out.println(s"document2map: ${document.uri}")
                ( document2tboxMap, d2map + document )
              case Success( Some( uri ) ) =>
                System.out.println(s"document2load: ${document.uri}\nresolved: $uri")
                loadTerminologyGraph( makeIRI( document.uri.toString ) ) match {
                  case Failure( t ) => 
                    System.out.println(s"*** document: ${document.uri}\n*** ${t.getClass.getName}: ${t.getMessage}")
                    t.printStackTrace(System.out)
                    //return Failure( t )
                    ( document2tboxMap, d2map + document )
                  case Success( tbox ) =>
                    System.out.println(s"==> document: ${document.uri}")
                    ( document2tboxMap + ( document -> tbox ), d2map )
                }
            }
        }

    System.out.println(s"**** Document2TBoxGraphCorrespondences( d2tbox=${document2tboxGraphs.size}, documents2map=${documents2map.size} )")
    Success( Document2TBoxGraphCorrespondences( resolved, document2tboxGraphs, documents2map ) )
  }

  def mapDocument2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF](
    correspondences: Document2TBoxGraphCorrespondences[Uml, Omf],
    document2BuiltInTBoxGraph: Function1[Document[Uml], Try[Option[Omf#ImmutableModelTerminologyGraph]]] )(
      implicit umlOps: UMLOps[Uml],
      omfOps: OMFOps[Omf],
      omfStore: Omf#Store,
      catalogIRIMapper: CatalogURIMapper ): Try[ResolvedDocumentSet2TBoxGraphMapping[Uml, Omf]] = ???

}