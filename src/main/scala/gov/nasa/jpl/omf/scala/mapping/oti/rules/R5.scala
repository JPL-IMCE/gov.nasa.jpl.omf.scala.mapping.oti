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

import java.lang.System

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti._

import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.trees._

import scala.{Some,StringContext}
import scala.collection.immutable._
import scalaz._

case class R5[Uml <: UML, Omf <: OMF, Provenance]()(implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf]) {

  def treeTypeMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._
    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (rule,
      ett@TboxUMLElementTreeType(Some(tbox), omfConcept, tree: TreeCompositeStructureType[Uml]),
      as, cs, rs, unmappedS) =>

        val result
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = if (TreeType.getIllFormedTreeBranchPairs(tree).nonEmpty) {
          System.out.println(s"*** Skip BST: $tree")
          \&/.That(
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults=Vector(),
              internalResults=Vector(),
              externalResults=Vector()))
        }
        else {
          System.out.println(s"*** Convert BST: $tree")
          \&/.That(
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults=Vector(),
              internalResults=Vector(),
              externalResults=Vector()))
        }

        result
    }

    MappingFunction[Uml, Omf, Provenance]("treeTypeMapping", mapping)

  }
}