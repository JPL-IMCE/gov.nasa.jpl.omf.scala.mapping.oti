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

import scala.{Some, StringContext}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.Predef.ArrowAssoc
import scalaz._, Scalaz._

/**
 * Mapping for a kind of binary, directed, composite UML Association to
 * an OMF relationship entity according to IMCE-generated profile stereotypes
 *
 * The UML association maps to an OMF entity relationship that specializes the OMF entity relationships
 * corresponding to the stereotypes applied.
 * If the UML association does not have any stereotype applied that maps
 * directly or indirectly to an OMF entity relationship,
 * then the mapping is equivalent to the mapping of the same UML association with
 * the IMCE-generated 'base:contains' stereotype applied.
 */
case class R4[Uml <: UML, Omf <: OMF, Provenance]()(implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf]) {

  def binaryCompositeAssociation2RelationshipMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._
    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (
        rule,
        TboxUMLElementTuple(Some(tbox), bcaU: UMLAssociation[Uml]),
        as, cs, rs, unmappedS)
        if cs.isEmpty && bcaU.memberEnd.exists(_.aggregation.contains(UMLAggregationKind.composite)) &&
          context.getDirectedBinaryAssociationSourceAndTargetMappings(bcaU).isDefined =>

        val ((targetTU, targetOmf), (sourceTU, sourceOmf))
        = context.getDirectedBinaryAssociationSourceAndTargetMappings(bcaU).get

        omfOps.foldTerm[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
          sourceOmf
        )(
          funEntityAspect = sourceDefinitionCompositeAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, bcaU, targetTU, targetOmf),
          funEntityConcept = sourceDefinitionCompositeAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, bcaU, targetTU, targetOmf),
          funEntityReifiedRelationship = sourceDefinitionCompositeAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, bcaU, targetTU, targetOmf),
          funEntityUnreifiedRelationship = illegalSourceAssociation2RelationshipMapping[Omf#ModelEntityUnreifiedRelationship](bcaU),
          funScalarDataType = illegalSourceAssociation2RelationshipMapping[Omf#ModelScalarDataType](bcaU),
          funStructuredDataType = illegalSourceAssociation2RelationshipMapping[Omf#ModelStructuredDataType](bcaU),
          funDataRelationshipFromEntityToScalar = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromEntityToScalar](bcaU),
          funDataRelationshipFromEntityToStructure = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromEntityToStructure](bcaU),
          funDataRelationshipFromStructureToScalar = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromStructureToScalar](bcaU),
          funDataRelationshipFromStructureToStructure = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromStructureToStructure](bcaU))
    }

    MappingFunction[Uml, Omf, Provenance]("binaryCompositeAssociation2RelationshipMapping", mapping)
  }

  def binaryReferenceAssociation2RelationshipMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._
    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (
        rule,
        TboxUMLElementTuple(Some(tbox), braU: UMLAssociation[Uml]),
        as, cs, rs, unmappedS)
        if cs.isEmpty && braU.memberEnd.forall(!_.aggregation.contains(UMLAggregationKind.composite)) &&
          context.getDirectedBinaryAssociationSourceAndTargetMappings(braU).isDefined =>

        val ((targetTU, targetOmf), (sourceTU, sourceOmf))
        = context.getDirectedBinaryAssociationSourceAndTargetMappings(braU).get

        omfOps.foldTerm[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
          sourceOmf
        )(
          funEntityAspect = sourceDefinitionReferenceAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, braU, targetTU, targetOmf),
          funEntityConcept = sourceDefinitionReferenceAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, braU, targetTU, targetOmf),
          funEntityReifiedRelationship = sourceDefinitionReferenceAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, braU, targetTU, targetOmf),
          funEntityUnreifiedRelationship = illegalSourceAssociation2RelationshipMapping[Omf#ModelEntityUnreifiedRelationship](braU),
          funScalarDataType = illegalSourceAssociation2RelationshipMapping[Omf#ModelScalarDataType](braU),
          funStructuredDataType = illegalSourceAssociation2RelationshipMapping[Omf#ModelStructuredDataType](braU),
          funDataRelationshipFromEntityToScalar = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromEntityToScalar](braU),
          funDataRelationshipFromEntityToStructure = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromEntityToStructure](braU),
          funDataRelationshipFromStructureToScalar = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromStructureToScalar](braU),
          funDataRelationshipFromStructureToStructure = illegalSourceAssociation2RelationshipMapping[Omf#ModelDataRelationshipFromStructureToStructure](braU))
    }

    MappingFunction[Uml, Omf, Provenance]("binaryReferenceAssociation2RelationshipMapping", mapping)
  }

  def illegalSourceAssociation2AspectMapping[Term <: Omf#ModelTypeTerm]
  (aU: UMLAssociation[Uml])
  ( other: Term )
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = \&/.This(
    Set(
      UMLError.illegalElementError[Uml, UMLAssociation[Uml]](
        s"R4 is not applicable to: $aU because its source is not mapped to an OMF Entity Concept",
        Iterable(aU))))

  def illegalSourceAssociation2RelationshipMapping[Term <: Omf#ModelTypeTerm]
  (aU: UMLAssociation[Uml])
  ( other: Term )
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = \&/.This(
    Set(
      UMLError.illegalElementError[Uml, UMLAssociation[Uml]](
        s"R4 is not applicable to: $aU because its source is not mapped to an OMF Entity Concept",
        Iterable(aU))))

  def sourceDefinitionCompositeAssociation2RelationshipMapping
  (rule: MappingFunction[Uml, Omf, Provenance],
   tbox: Omf#MutableModelTerminologyGraph,
   context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   rs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]],
   sourceTU: UMLClassifier[Uml],
   aU: UMLAssociation[Uml],
   targetTU: UMLClassifier[Uml],
   targetOmf: Omf#ModelEntityDefinition)
  ( sourceOmf: Omf#ModelEntityDefinition)
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = {

    val unmappedErrors
    : Set[java.lang.Throwable]
    = unmappedS.map { s =>
      require(context.otherStereotypesApplied.contains(s), s.qualifiedName.get)

      UMLError.illegalElementError[Uml, UMLAssociation[Uml]](
        s"R4 unmapped non-IMCE stereotype application: <<${s.qualifiedName.get}>>$aU",
        Iterable(aU))
    }

    val sourceName = sourceTU.name.getOrElse(sourceTU.toolSpecific_id)
    val targetName = targetTU.name.getOrElse(sourceTU.toolSpecific_id)
    val hasName = aU.name

    val omfRelationshipParents
    = if (rs.isEmpty)
      Map(context.baseContainsS -> context.baseContainsR)
    else
      rs

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

    val result
    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
    = for {
      contexts <-
      omfRelationshipParents
        .foldLeft[Set[java.lang.Throwable] \&/ Vector[TboxUMLElement2ReifiedRelationshipContextualization[Uml, Omf]]](\&/.That(Vector.empty)) {
        case (acc, (relS, relO)) =>
          val contextName = hasName.getOrElse(sourceName + "_" + relS.name.get + "_" + targetName)
          val ax =
            context
              .addEntityDefinitionExistentialRestrictionAxiom(
                rule,
                aU, s"R4.sourceDefinitionCompositeAssociation2RelationshipMapping(${relS.qualifiedName.get})",
                tbox, aU, relS, sourceOmf, relO, targetOmf)
          val inc =
            ax
              .map(_ => Vector(TboxUMLElement2ReifiedRelationshipContextualization(
                Some(tbox), relO, aU, sourceTU, sourceOmf, targetTU, targetOmf, contextName)))
              .toThese

          acc append inc
      }
    } yield {
      RuleResult[Uml, Omf, Provenance](
        rule,
        finalResults = contexts,
        internalResults = Vector(),
        externalResults = Vector())
    }

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

  def sourceDefinitionReferenceAssociation2RelationshipMapping
  (rule: MappingFunction[Uml, Omf, Provenance],
   tbox: Omf#MutableModelTerminologyGraph,
   context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   rs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]],
   sourceTU: UMLClassifier[Uml],
   aU: UMLAssociation[Uml],
   targetTU: UMLClassifier[Uml],
   targetOmf: Omf#ModelEntityDefinition)
  ( sourceOmf: Omf#ModelEntityDefinition)
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = {

    val unmappedErrors
    : Set[java.lang.Throwable]
    = unmappedS.map { s =>
      require(context.otherStereotypesApplied.contains(s), s.qualifiedName.get)

      UMLError.illegalElementError[Uml, UMLAssociation[Uml]](
        s"R4 unmapped non-IMCE stereotype application: <<${s.qualifiedName.get}>>$aU",
        Iterable(aU))
    }

    val sourceName = sourceTU.name.getOrElse(sourceTU.toolSpecific_id)
    val targetName = targetTU.name.getOrElse(sourceTU.toolSpecific_id)
    val hasName = aU.name

    val omfRelationshipParents
    = if (rs.isEmpty)
      Map(context.baseAggregatesS -> context.baseAggregatesR)
    else
      rs

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

    val result
    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
    = for {
      contexts <-
      omfRelationshipParents
        .foldLeft[Set[java.lang.Throwable] \&/ Vector[TboxUMLElement2ReifiedRelationshipContextualization[Uml, Omf]]](\&/.That(Vector.empty)) {
        case (acc, (relS, relO)) =>
          val contextName = hasName.getOrElse(sourceName + "_" + relS.name.get + "_" + targetName)
          val ax =
            context
              .addEntityDefinitionExistentialRestrictionAxiom(
                rule,
                aU, s"R4.sourceDefinitionReferenceAssociation2RelationshipMapping(${relS.qualifiedName.get})",
                tbox, aU, relS, sourceOmf, relO, targetOmf)
          val inc =
            ax
              .map(_ => Vector(TboxUMLElement2ReifiedRelationshipContextualization(
                Some(tbox), relO, aU, sourceTU, sourceOmf, targetTU, targetOmf, contextName)))
              .toThese

          acc append inc
      }
    } yield {
      RuleResult[Uml, Omf, Provenance](
        rule,
        finalResults = contexts,
        internalResults = Vector(),
        externalResults = Vector())
    }

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