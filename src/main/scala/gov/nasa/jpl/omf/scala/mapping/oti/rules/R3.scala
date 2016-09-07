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