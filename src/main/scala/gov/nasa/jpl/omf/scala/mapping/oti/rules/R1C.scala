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

package gov.nasa.jpl.omf.scala.mapping.oti.rules

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti._
import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.collection.immutable._
import scalaz._, Scalaz._

case class R1C[Uml <: UML, Omf <: OMF, Provenance]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  def packageContentsMapping
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance) = {

    val mapping
    : OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction
    = {

      case (rule, pair: TBoxPackageContentsConversion[Uml, Omf], as, cs, rs, unmappedS) => {
        val contents =
          pair.e.packagedElement.filter {
          case _: UMLPackage[Uml] =>
            false
          case _: UMLPackageImport[Uml] =>
            false
          case _: UMLElementImport[Uml] =>
            false
          case _: UMLPackageMerge[Uml] =>
            false
          case e =>
            ! context.ignoreCrossReferencedElementFilter(e)
        }

        \&/.That(RuleResult[Uml, Omf, Provenance](
          rule,
          finalResults = Vector(),
          internalResults = Vector(),
          externalResults = contents.map(TboxUMLElementTuple(pair.pkgDocumentTbox.some, _)).toVector))
      }
    }

    MappingFunction[Uml, Omf, Provenance]("R1B (packageContentsMapping)", mapping)
  }


}