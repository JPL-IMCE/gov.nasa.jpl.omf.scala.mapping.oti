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
package gov.nasa.jpl.omf.scala.mapping.oti.rules

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.language.postfixOps

/**
 * Mapping for a kind of UML Dependency to an OMF relationship entity according to IMCE-generated profile stereotypes
 *
 * There must at least 1 stereotype applied to the dependency that maps directly or indirectly to
 * a kind of OMF relationship entity.
 *
 * The UML dependency maps to an OMF entity relationship that specializes
 * the OMF entity relationships corresponding to the stereotypes applied.
 */
case class R3[Uml <: UML, Omf <: OMF]()(implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf]) {

  def dependency2RelationshipMapping(context: OTI2OMFMappingContext[Uml, Omf]) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction = {
      case (rule, TboxUMLElementTuple(Some(tbox), depU: UMLDependency[Uml]), as, cs, rs, unmappedS)
        if rs.nonEmpty && context.getDependencySourceAndTargetMappings(depU).isDefined =>

        if (unmappedS.nonEmpty) {
          val foreign = unmappedS.filter(!context.otherStereotypesApplied.contains(_))
          require(foreign.isEmpty)
        }

        val ((sourceU, sourceOmf), (targetU, targetOmf)) =
          context.getDependencySourceAndTargetMappings(depU).get

        val r1 =
          if (rs.size == 1) Some(rs.head._1) else None

        val r1Name =
          if (r1.isDefined) r1.get.name.get else ""

        val hasName =
          sourceU.name.getOrElse(sourceU.id) + "-" + r1Name + "-" + targetU.name.getOrElse(targetU.id)

        for {
          depOmfRelation <- context.mapElement2Relationship(
            rule, tbox, depU, sourceOmf, targetOmf,
            Iterable(), // @TODO
            isAbstract = false,
            Some(hasName))

          _ = rs.foreach {
            case (relUml, relOmf) =>
              context.addEntityRelationshipSubClassAxiom(rule, tbox, depOmfRelation, relOmf).get
          }

        } yield Tuple2(Nil, Nil)
    }

    MappingFunction[Uml, Omf]("dependency2RelationshipMapping", mapping)

  }
}