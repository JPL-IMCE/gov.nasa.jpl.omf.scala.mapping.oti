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

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{Option, Some, StringContext, Tuple3, Unit}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.language.postfixOps
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
        if cs.isEmpty && bcaU.memberEnd.exists(_.aggregation == UMLAggregationKind.composite) &&
          context.getDirectedBinaryAssociationSourceAndTargetMappings(bcaU).isDefined =>

        val unmappedErrors
        : Set[java.lang.Throwable]
        = unmappedS.map { s =>
          require(context.otherStereotypesApplied.contains(s), s.qualifiedName.get)

          UMLError.illegalElementError[Uml, UMLAssociation[Uml]](
            s"R4 unmapped non-IMCE stereotype application: <<${s.qualifiedName.get}>>$bcaU",
            Iterable(bcaU))
        }

        val ((sourceTU, sourceOmf), (targetTU, targetOmf))
        = context.getDirectedBinaryAssociationSourceAndTargetMappings(bcaU).get

        val sourceName = sourceTU.name.get
        val targetName = targetTU.name.get
        val hasName = bcaU.name

        val omfRelationshipParents
        = if (rs.isEmpty)
          Map(context.baseContainsS -> context.baseContainsR)
        else
          rs

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
                  .addEntityRelationshipContextualizationAxiom(
                    rule, tbox, bcaU, relS, sourceOmf, relO, contextName, targetOmf)
              val inc =
                ax
                  .map(_ => Vector(TboxUMLElement2ReifiedRelationshipContextualization(
                    Some(tbox), relO, bcaU, sourceTU, sourceOmf, targetTU, targetOmf, contextName)))
                  .toThese

              acc append inc
          }
        } yield {
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults=contexts,
              internalResults=Vector(),
              externalResults=Vector())
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

    MappingFunction[Uml, Omf, Provenance]("binaryCompositeAssociation2RelationshipMapping", mapping)

  }

  def binaryReferenceAssociation2RelationshipMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._
    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (
        rule,
        TboxUMLElementTuple(Some(tbox), braU: UMLAssociation[Uml]),
        as, cs, rs, unmappedS)
        if cs.isEmpty && braU.memberEnd.forall(_.aggregation != UMLAggregationKind.composite) &&
          context.getDirectedBinaryAssociationSourceAndTargetMappings(braU).isDefined =>

        val unmappedErrors
        : Set[java.lang.Throwable]
        = unmappedS.map { s =>
          require(context.otherStereotypesApplied.contains(s), s.qualifiedName.get)

          UMLError.illegalElementError[Uml, UMLAssociation[Uml]](
            s"R4 unmapped non-IMCE stereotype application: <<${s.qualifiedName.get}>>$braU",
            Iterable(braU))
        }

        val ((sourceTU, sourceOmf), (targetTU, targetOmf))
        = context.getDirectedBinaryAssociationSourceAndTargetMappings(braU).get

        val sourceName = sourceTU.name.get
        val targetName = targetTU.name.get
        val hasName = braU.name

        val omfRelationshipParents
        = if (rs.isEmpty)
          Map(context.baseContainsS -> context.baseContainsR)
        else
          rs

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
                  .addEntityRelationshipContextualizationAxiom(
                    rule, tbox, braU, relS, sourceOmf, relO, contextName, targetOmf)
              val inc =
                ax
                  .map(_ => Vector(TboxUMLElement2ReifiedRelationshipContextualization(
                    Some(tbox), relO, braU, sourceTU, sourceOmf, targetTU, targetOmf, contextName)))
                  .toThese

              acc append inc
          }
        } yield {
          RuleResult[Uml, Omf, Provenance](
            rule,
            finalResults=contexts,
            internalResults=Vector(),
            externalResults=Vector())
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

    MappingFunction[Uml, Omf, Provenance]("binaryReferenceAssociation2RelationshipMapping", mapping)

  }
}