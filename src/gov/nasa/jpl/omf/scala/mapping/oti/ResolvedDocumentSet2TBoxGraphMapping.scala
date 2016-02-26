/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
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

import java.lang.System

import gov.nasa.jpl.omf.scala.core._
import org.omg.oti.uml.OTIPrimitiveTypes._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.xmi._
import org.omg.oti.uml.canonicalXMI._

import scala.{Function1,Option,StringContext}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scalaz._, Scalaz._

case class ResolvedDocumentSet2TBoxGraphMapping[Uml <: UML, Omf <: OMF]()(
  implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

case class ResolvedDocumentTBoxGraph[Uml <: UML, Omf <: OMF]
( document: Document[Uml],
  tbox: Omf#ImmutableModelTerminologyGraph,
  aspects: Map[UMLElement[Uml], Omf#ModelEntityAspect],
  concepts: Map[UMLElement[Uml], Omf#ModelEntityConcept] )
( implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

case class Document2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF]
( resolved: ResolvedDocumentSet[Uml],
  document2tboxGraphs: Map[Document[Uml], Omf#ImmutableModelTerminologyGraph],
  documents2map: Set[Document[Uml]] )
( implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

object ResolvedDocumentSet2TBoxGraphMapping {

  def resolvedDocumentSet2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF]
  ( resolved: ResolvedDocumentSet[Uml] )
  ( implicit umlOps: UMLOps[Uml],
    omfOps: OMFOps[Omf],
    omfStore: Omf#Store,
    catalogIRIMapper: CatalogURIMapper )
  : Set[java.lang.Throwable] \/ Document2TBoxGraphCorrespondences[Uml, Omf] = {

    val sortedDocuments =
      resolved
        .ds
        .topologicalSort( resolved.g )
        .fold[List[Document[Uml]]](
          ( _d: Document[Uml] )         => _d :: Nil,
          ( _ds: List[Document[Uml]] )  => _ds
        )

    import omfOps._

    type DocumentGraphMap = ( Map[Document[Uml], Omf#ImmutableModelTerminologyGraph], Set[Document[Uml]] )
    val m0: Set[java.lang.Throwable] \/ DocumentGraphMap =
      (Map[Document[Uml], Omf#ImmutableModelTerminologyGraph](), Set[Document[Uml]]()).right
    val mN: Set[java.lang.Throwable] \/ DocumentGraphMap =
      (m0 /: sortedDocuments ) {
        (mi, document) =>
          \/.fromTryCatchNonFatal[java.net.URI](new java.net.URI(OTI_URI.unwrap(document.info.packageURI)))
            .fold[Set[java.lang.Throwable] \/ DocumentGraphMap](
            l = (t: java.lang.Throwable) =>
              -\/(
                Set(
                  UMLError.illegalElementException[Uml, UMLPackage[Uml]](
                    s"Cannnot create a valid URI for the package's document URL: ${document.info}",
                    Iterable(document.scope),
                    t))),
            r = (duri: java.net.URI) => {
              catalogIRIMapper
                .resolveURI(duri, catalogIRIMapper.loadResolutionStrategy(".owl".some))
                .leftMap[Set[java.lang.Throwable]](_.toList.toSet)
                .flatMap {
                  _.fold[Set[java.lang.Throwable] \/ DocumentGraphMap](
                    mi.map { case (document2tboxMap, d2map) =>
                      System.out.println(s"document2map: $duri")
                      (document2tboxMap, d2map + document)
                    }
                  ) { uri =>
                    System.out.println(s"document2load: $duri\nresolved: $uri")
                    mi.flatMap { case (document2tboxMap, d2map) =>
                      // @todo it seems this should use the resolved uri instead of document.uri
                      makeIRI(uri.toString)
                        .leftMap[Set[java.lang.Throwable]](_.toList.toSet)
                        .flatMap { iri =>
                          loadTerminologyGraph(iri)
                            .leftMap[Set[java.lang.Throwable]](_.toList.toSet)
                            .flatMap { case (iTbox, _) =>
                              System.out.println(s"==> document: $duri")
                              (document2tboxMap + (document -> iTbox), d2map).right
                            }
                        }
                    }
                  }
                }
            }
          )
      }

    mN.map { case (document2tboxGraphs, documents2map) =>
      System.out.println(s"**** Document2TBoxGraphCorrespondences( d2tbox=${document2tboxGraphs.size}, documents2map=${documents2map.size} )")
      Document2TBoxGraphCorrespondences(resolved, document2tboxGraphs, documents2map)
    }
  }

  def mapDocument2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF](
    correspondences: Document2TBoxGraphCorrespondences[Uml, Omf],
    document2BuiltInTBoxGraph: Function1[Document[Uml], Set[java.lang.Throwable] \/ Option[Omf#ImmutableModelTerminologyGraph]] )(
      implicit umlOps: UMLOps[Uml],
      omfOps: OMFOps[Omf],
      omfStore: Omf#Store,
      catalogIRIMapper: CatalogURIMapper ): Set[java.lang.Throwable] \/ ResolvedDocumentSet2TBoxGraphMapping[Uml, Omf] = ???

}