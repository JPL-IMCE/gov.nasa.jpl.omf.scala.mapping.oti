/*
 * Copyright 2016 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package gov.nasa.jpl.omf.scala.mapping.oti

import java.lang.System

import gov.nasa.jpl.omf.scala.core._
import org.omg.oti.json.common.OTIPrimitiveTypes._
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
  tbox: Omf#ImmutableTerminologyBox,
  aspects: Map[UMLElement[Uml], Omf#Aspect],
  concepts: Map[UMLElement[Uml], Omf#Concept] )
( implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

case class Document2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF]
( resolved: ResolvedDocumentSet[Uml],
  document2tboxGraphs: Map[Document[Uml], Omf#ImmutableTerminologyBox],
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

    type DocumentGraphMap = ( Map[Document[Uml], Omf#ImmutableTerminologyBox], Set[Document[Uml]] )
    val m0: Set[java.lang.Throwable] \/ DocumentGraphMap =
      (Map[Document[Uml], Omf#ImmutableTerminologyBox](), Set[Document[Uml]]()).right
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
                        .flatMap { iri =>
                          loadTerminology(iri)
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
    document2BuiltInTBoxGraph: Function1[Document[Uml], Set[java.lang.Throwable] \/ Option[Omf#ImmutableTerminologyBox]] )(
      implicit umlOps: UMLOps[Uml],
      omfOps: OMFOps[Omf],
      omfStore: Omf#Store,
      catalogIRIMapper: CatalogURIMapper ): Set[java.lang.Throwable] \/ ResolvedDocumentSet2TBoxGraphMapping[Uml, Omf] = ???

}