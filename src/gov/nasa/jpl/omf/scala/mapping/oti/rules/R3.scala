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

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti._

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{Option,Some,StringContext}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scalaz._, Scalaz._

/**
 * Mapping for a kind of UML Dependency to an OMF relationship entity according to IMCE-generated profile stereotypes
 *
 * There must at least 1 stereotype applied to the dependency that maps directly or indirectly to
 * a kind of OMF relationship entity.
 *
 * The UML dependency maps to an OMF entity relationship that specializes
 * the OMF entity relationships corresponding to the stereotypes applied.
 */
case class R3[Uml <: UML, Omf <: OMF, Provenance]()(implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf]) {


  def dependency2RelationshipMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (rule, TboxUMLElementTuple(Some(tbox), depU: UMLDependency[Uml]), as, cs, rs, unmappedS) =>
        if (rs.isEmpty) {

          val explanation: String =
            "No relationship-mapped stereotypes applied" +
              (if (unmappedS.isEmpty)
                ""
              else
                unmappedS.toList.map(s => s.qualifiedName.getOrElse(s.toolSpecific_id))
                  .mkString(s", and ${unmappedS.size} unmapped stereotypes applied (",",",")"))

          \&/.This(Set(
            UMLError.illegalElementError[Uml, UMLDependency[Uml]](
              s"R3 is not applicable to: $depU because $explanation",
              Iterable(depU))
          ))

        } else {
          val ( ( sourceU, osourceE ), ( targetU, otargetE )) =
            context.getDependencySourceAndTargetMappings(depU)

          osourceE
          .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]]{

            val explanation: String =
              s"R3 dependency2RelationshipMapping => unmapped source: "+
              s"${sourceU.toolSpecific_id} ${sourceU.xmiElementLabel} ${sourceU.qualifiedName.getOrElse(sourceU.toolSpecific_id)}"+
              s"(target? ${otargetE.isDefined}): ${depU.toolSpecific_id} ${depU.xmiElementLabel})"
            \&/.This(Set(
              UMLError.illegalElementError[Uml, UMLDependency[Uml]](
                s"R3 is not applicable to: $depU because $explanation",
                Iterable(depU))))

          } { sourceOmf =>

            omfOps.foldTerm[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
              sourceOmf
            )(funEntityAspect = sourceDefinitionDependency2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceU, depU, targetU, otargetE),
              funEntityConcept = sourceDefinitionDependency2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceU, depU, targetU, otargetE),
              funEntityReifiedRelationship = sourceDefinitionDependency2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceU, depU, targetU, otargetE),
              funEntityUnreifiedRelationship = illegalSourceDependency2RelationshipMapping[Omf#ModelEntityUnreifiedRelationship](depU),
              funScalarDataType = illegalSourceDependency2RelationshipMapping[Omf#ModelScalarDataType](depU),
              funStructuredDataType = illegalSourceDependency2RelationshipMapping[Omf#ModelStructuredDataType](depU),
              funDataRelationshipFromEntityToScalar = illegalSourceDependency2RelationshipMapping[Omf#ModelDataRelationshipFromEntityToScalar](depU),
              funDataRelationshipFromEntityToStructure = illegalSourceDependency2RelationshipMapping[Omf#ModelDataRelationshipFromEntityToStructure](depU),
              funDataRelationshipFromStructureToScalar = illegalSourceDependency2RelationshipMapping[Omf#ModelDataRelationshipFromStructureToScalar](depU),
              funDataRelationshipFromStructureToStructure = illegalSourceDependency2RelationshipMapping[Omf#ModelDataRelationshipFromStructureToStructure](depU))

          }
        }
    }

    MappingFunction[Uml, Omf, Provenance]("dependency2RelationshipMapping", mapping)

  }

  def illegalSourceDependency2AspectMapping[Term <: Omf#ModelTypeTerm]
  (depU: UMLDependency[Uml])
  ( other: Term )
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = \&/.This(
    Set(
      UMLError.illegalElementError[Uml, UMLDependency[Uml]](
        s"R3 is not applicable to: $depU because its source is not mapped to an OMF Entity Concept",
        Iterable(depU))))

  def illegalSourceDependency2RelationshipMapping[Term <: Omf#ModelTypeTerm]
  (depU: UMLDependency[Uml])
  ( other: Term )
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = \&/.This(
    Set(
      UMLError.illegalElementError[Uml, UMLDependency[Uml]](
        s"R3 is not applicable to: $depU because its source is not mapped to an OMF Entity Concept",
        Iterable(depU))))

  def sourceDefinitionDependency2RelationshipMapping
  (rule: MappingFunction[Uml, Omf, Provenance],
   tbox: Omf#MutableModelTerminologyGraph,
   context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   rs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]],
   sourceU: UMLNamedElement[Uml],
   depU: UMLDependency[Uml],
   targetU: UMLNamedElement[Uml],
   otargetE: Option[Omf#ModelEntityDefinition])
  ( sourceOmf: Omf#ModelEntityDefinition)
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = {

    otargetE
      .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]]{

      val explanation: String =
        s"R3 dependency2RelationshipMapping => unmapped target: "+
          s"${targetU.toolSpecific_id} ${targetU.xmiElementLabel} ${targetU.qualifiedName.getOrElse(targetU.toolSpecific_id)}" +
          s"(source? true): ${depU.toolSpecific_id} ${depU.xmiElementLabel})"
      \&/.This(Set(
        UMLError.illegalElementError[Uml, UMLDependency[Uml]](
          s"R3 is not applicable to: $depU because $explanation",
          Iterable(depU))))

    } { targetOmf =>

      import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

      val unmappedErrors
      : Set[java.lang.Throwable]
      = unmappedS.map { s =>
        require(context.otherStereotypesApplied.contains(s), s.qualifiedName.get)

        UMLError.illegalElementError[Uml, UMLDependency[Uml]](
          s"R3 unmapped non-IMCE stereotype application: <<${s.qualifiedName.get}>>$depU",
          Iterable(depU))
      }

      val rName = rs.flatMap(_._1.name).mkString("<<", ",", ">>")

      val hasName =
        sourceU.name.getOrElse(sourceU.toolSpecific_id) +
          "-" + rName + "-" +
          targetU.name.getOrElse(targetU.toolSpecific_id)

      val result = for {
        restrictions <-
        rs
          .foldLeft[Set[java.lang.Throwable] \&/ Vector[TboxUMLElement2ReifiedRelationshipRestriction[Uml, Omf]]](\&/.That(Vector.empty)) {
          case (acc, (relUml, relOmf)) =>
            val ax =
              context
                .addEntityDefinitionExistentialRestrictionAxiom(
                  rule, tbox, depU, relUml, sourceOmf, relOmf, targetOmf)
            val inc =
              ax
                .map(_ => Vector(TboxUMLElement2ReifiedRelationshipRestriction(
                  Some(tbox), relOmf, depU, sourceU, sourceOmf, targetU, targetOmf, ExistentialRestrictionKind)))
                .toThese

            acc append inc
        }
      } yield
        RuleResult[Uml, Omf, Provenance](
          rule,
          finalResults=restrictions,
          internalResults=Vector(),
          externalResults=Vector()) // nothing further to do

      if (unmappedErrors.isEmpty)
        result
      else
        result match {
          case \&/.This(errors) =>
            \&/.This(errors ++ unmappedErrors)
          case \&/.That(r) =>
            \&/.Both(unmappedErrors, r)
          case \&/.Both(errors, r) =>
            \&/.Both(errors ++ unmappedErrors, r)
        }
    }
  }
}