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