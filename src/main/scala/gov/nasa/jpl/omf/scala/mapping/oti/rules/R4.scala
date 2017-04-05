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

import org.omg.oti.json.common.OTIPrimitiveTypes.TOOL_SPECIFIC_ID
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
          funAspect = sourceDefinitionCompositeAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, bcaU, targetTU, targetOmf),
          funConcept = sourceDefinitionCompositeAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, bcaU, targetTU, targetOmf),
          funReifiedRelationship = sourceDefinitionCompositeAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, bcaU, targetTU, targetOmf),
          funUnreifiedRelationship = illegalSourceAssociation2RelationshipMapping[Omf#UnreifiedRelationship](bcaU),
          funScalar = illegalSourceAssociation2RelationshipMapping[Omf#Scalar](bcaU),
          funStructure = illegalSourceAssociation2RelationshipMapping[Omf#Structure](bcaU),
          funScalarOneOfRestriction = illegalSourceAssociation2RelationshipMapping[Omf#ScalarOneOfRestriction](bcaU),
          funBinaryScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#BinaryScalarRestriction](bcaU),
          funIRIScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#IRIScalarRestriction](bcaU),
          funPlainLiteralScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#PlainLiteralScalarRestriction](bcaU),
          funStringScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#StringScalarRestriction](bcaU),
          funSynonymScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#SynonymScalarRestriction](bcaU),
          funTimeScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#TimeScalarRestriction](bcaU),
          funEntityScalarDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#EntityScalarDataProperty](bcaU),
          funEntityStructuredDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#EntityStructuredDataProperty](bcaU),
          funScalarDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#ScalarDataProperty](bcaU),
          funStructuredDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#StructuredDataProperty](bcaU))(
          sourceOmf
        )
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
          funAspect = sourceDefinitionReferenceAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, braU, targetTU, targetOmf),
          funConcept = sourceDefinitionReferenceAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, braU, targetTU, targetOmf),
          funReifiedRelationship = sourceDefinitionReferenceAssociation2RelationshipMapping(rule, tbox, context, rs, unmappedS, sourceTU, braU, targetTU, targetOmf),
          funUnreifiedRelationship = illegalSourceAssociation2RelationshipMapping[Omf#UnreifiedRelationship](braU),
          funScalar = illegalSourceAssociation2RelationshipMapping[Omf#Scalar](braU),
          funStructure = illegalSourceAssociation2RelationshipMapping[Omf#Structure](braU),
          funScalarOneOfRestriction = illegalSourceAssociation2RelationshipMapping[Omf#ScalarOneOfRestriction](braU),
          funBinaryScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#BinaryScalarRestriction](braU),
          funIRIScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#IRIScalarRestriction](braU),
          funPlainLiteralScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#PlainLiteralScalarRestriction](braU),
          funStringScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#StringScalarRestriction](braU),
          funSynonymScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#SynonymScalarRestriction](braU),
          funTimeScalarRestriction = illegalSourceAssociation2RelationshipMapping[Omf#TimeScalarRestriction](braU),
          funEntityScalarDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#EntityScalarDataProperty](braU),
          funEntityStructuredDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#EntityStructuredDataProperty](braU),
          funScalarDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#ScalarDataProperty](braU),
          funStructuredDataProperty = illegalSourceAssociation2RelationshipMapping[Omf#StructuredDataProperty](braU))(
          sourceOmf
        )
    }

    MappingFunction[Uml, Omf, Provenance]("binaryReferenceAssociation2RelationshipMapping", mapping)
  }

  def illegalSourceAssociation2AspectMapping[Term <: Omf#Term]
  (aU: UMLAssociation[Uml])
  ( other: Term )
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = \&/.This(
    Set(
      UMLError.illegalElementError[Uml, UMLAssociation[Uml]](
        s"R4 is not applicable to: $aU because its source is not mapped to an OMF Entity Concept",
        Iterable(aU))))

  def illegalSourceAssociation2RelationshipMapping[Term <: Omf#Term]
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
   tbox: Omf#MutableTerminologyBox,
   context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   rs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]],
   sourceTU: UMLClassifier[Uml],
   aU: UMLAssociation[Uml],
   targetTU: UMLClassifier[Uml],
   targetOmf: Omf#Entity)
  ( sourceOmf: Omf#Entity)
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

    val sourceName = sourceTU.name.getOrElse(TOOL_SPECIFIC_ID.unwrap(sourceTU.toolSpecific_id))
    val targetName = targetTU.name.getOrElse(TOOL_SPECIFIC_ID.unwrap(targetTU.toolSpecific_id))
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
   tbox: Omf#MutableTerminologyBox,
   context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   rs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]],
   sourceTU: UMLClassifier[Uml],
   aU: UMLAssociation[Uml],
   targetTU: UMLClassifier[Uml],
   targetOmf: Omf#Entity)
  ( sourceOmf: Omf#Entity)
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

    val sourceName = sourceTU.name.getOrElse(TOOL_SPECIFIC_ID.unwrap(sourceTU.toolSpecific_id))
    val targetName = targetTU.name.getOrElse(TOOL_SPECIFIC_ID.unwrap(sourceTU.toolSpecific_id))
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