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
package gov.nasa.jpl.omf.scala.mapping.oti

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.trees._
import org.omg.oti.uml.xmi._

import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.{annotation,Boolean,Enumeration,Function1,Function2,Function3,Function4,Function5,Function8}
import scala.{Option,None,PartialFunction,Some,StringContext,Tuple2,Tuple6,Unit}
import scala.language.postfixOps
import scalaz._, Scalaz._

object Namespace2OMFTypeTermKind extends Enumeration {
  type Namespace2OMFTypeTermKind = Value
  val Aspect = Value
  val Concept = Value
  val Relationship = Value
}

trait Namespace2TBoxLookupFunction[Uml <: UML, Omf <: OMF]
  extends Function2[UMLNamespace[Uml], TerminologyKind, Option[Omf#ModelTerminologyGraph]]

trait Element2AspectCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect
}

trait Element2AspectCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2AspectCTor[Uml, Omf]
  with OTI2OMFMappingContext[Uml, Omf]#Element2AspectCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect =
  for {
    aspect <- apply(rule, tbox, u)
    _ = context.mappedElement2Aspect += (u -> aspect)
  } yield aspect

}

trait Element2ConceptCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean)
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf]#MappedEntityConcept
}

trait Element2ConceptCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2ConceptCTor[Uml, Omf]
  with OTI2OMFMappingContext[Uml, Omf]#Element2ConceptCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean)
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf]#MappedEntityConcept =
  for {
    conceptGraph <- apply(rule, tbox, u, isAbstract)
    _ = context.mappedElement2Concept += (u -> conceptGraph)
  } yield conceptGraph

}

trait Element2RelationshipCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: Option[String] )
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf]#MappedEntityRelationship
}

trait Element2RelationshipCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2RelationshipCTor[Uml, Omf]
  with OTI2OMFMappingContext[Uml, Omf]#Element2RelationshipCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: Option[String] )
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf]#MappedEntityRelationship =
  for {
    relationship <- apply(rule, tbox, u, source, target, characteristics, isAbstract, name )
    _ = context.mappedElement2Relationship += (u -> relationship)
  } yield relationship
}

sealed abstract class TboxUMLElementPair[Uml <: UML, Omf <: OMF]
( val tbox: Option[Omf#MutableModelTerminologyGraph],
  val e: UMLElement[Uml] )
( implicit omfOps: OMFOps[Omf] )

case class TboxUMLElementTuple[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val e: UMLElement[Uml] )
( implicit omfOps: OMFOps[Omf] )
extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

  override def toString: String =
    tbox
    .fold[String](
      s"Tuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
        s"Tuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    }
}

case class TboxUMLElementTreeType[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  bstConcept: Omf#ModelEntityConcept,
  tree: TreeType[Uml])
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, tree.treeFeatureType ) {

  override val e = tree.treeFeatureType

  override def toString: String =
    tbox
      .fold[String](
        s"Tree[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
      ){ g =>
        s"Tree[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
      }
}

case class TboxUMLElementTreeTypedFeatureBranchType[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val e: UMLType[Uml],
  omfBSTConcept: Omf#ModelEntityConcept,
  branch: TreeTypedFeatureBranch[Uml])
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

  override def toString: String =
    tbox
      .fold[String](
        s"Branch[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
      ){ g =>
        s"Branch[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
      }
}

case class MappingFunction[Uml <: UML, Omf <: OMF]
( name: String,
  mappingRule: OTI2OMFMappingContext[Uml, Omf]#RuleFunction )
( implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] )

trait Namespace2TBoxCtor[Uml <: UML, Omf <: OMF]
  extends Function3[
    MappingFunction[Uml, Omf],
    UMLNamespace[Uml],
    TerminologyKind,
    NonEmptyList[java.lang.Throwable] \/ Omf#MutableModelTerminologyGraph]

trait AddDirectlyNestedTerminologyGraph[Uml <: UML, Omf <: OMF]
  extends Function3[
    MappingFunction[Uml, Omf],
    Omf#MutableModelTerminologyGraph,
    Omf#MutableModelTerminologyGraph,
    NonEmptyList[java.lang.Throwable] \/ Unit]

trait AddEntityDefinitionAspectSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[
    MappingFunction[Uml, Omf],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityDefinition,
    Omf#ModelEntityAspect,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityDefinitionAspectSubClassAxiom]

trait AddEntityConceptSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[
    MappingFunction[Uml, Omf],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityConcept,
    Omf#ModelEntityConcept,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityConceptSubClassAxiom]

trait AddEntityReifiedRelationshipSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[
    MappingFunction[Uml, Omf],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityReifiedRelationship,
    Omf#ModelEntityReifiedRelationship,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityReifiedRelationshipSubClassAxiom]

trait AddEntityConceptDesignationTerminologyGraphAxiom[Uml <: UML, Omf <: OMF]
  extends Function5[
    MappingFunction[Uml, Omf],
    TreeCompositeStructureType[Uml],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityConcept,
    Omf#MutableModelTerminologyGraph,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityConceptDesignationTerminologyGraphAxiom]

case class OTI2OMFMappingContext[Uml <: UML, Omf <: OMF]
( ignoreCrossReferencedElementFilter: Function1[UMLElement[Uml], Boolean],
  iriPrefix: String,
  tboxLookup: Namespace2TBoxLookupFunction[Uml, Omf],
  ns2tboxCtor: Namespace2TBoxCtor[Uml, Omf],

  protected val element2aspectCtor: Element2AspectCTor[Uml, Omf],
  protected val element2conceptCtor: Element2ConceptCTor[Uml, Omf],
  protected val element2relationshipCtor: Element2RelationshipCTor[Uml, Omf],

  addDirectlyNestedTerminologyGraph: AddDirectlyNestedTerminologyGraph[Uml, Omf],
  addEntityDefinitionAspectSubClassAxiom: AddEntityDefinitionAspectSubClassAxiom[Uml, Omf],
  addEntityConceptSubClassAxiom: AddEntityConceptSubClassAxiom[Uml, Omf],
  addEntityRelationshipSubClassAxiom: AddEntityReifiedRelationshipSubClassAxiom[Uml, Omf],
  addEntityConceptDesignationTerminologyGraphAxiom: AddEntityConceptDesignationTerminologyGraphAxiom[Uml, Omf],

  stereotype2Aspect: Map[UMLStereotype[Uml], Omf#ModelEntityAspect],
  stereotype2Concept: Map[UMLStereotype[Uml], Omf#ModelEntityConcept],
  stereotype2Relationship: Map[UMLStereotype[Uml], Omf#ModelEntityReifiedRelationship],
  otherStereotypesApplied: Set[UMLStereotype[Uml]],
  ops: OMFOps[Omf],
  treeOps: TreeOps[Uml],
  idg: IDGenerator[Uml]) {

  implicit val umlOps = idg.umlOps
  import umlOps._
  import ops._

  type UMLStereotype2EntityAspectMap = Map[UMLStereotype[Uml], Omf#ModelEntityAspect]
  type UMLStereotype2EntityConceptMap = Map[UMLStereotype[Uml], Omf#ModelEntityConcept]
  type UMLStereotype2EntityRelationshipMap = Map[UMLStereotype[Uml], Omf#ModelEntityReifiedRelationship]
  type TboxUMLElementPairs = List[TboxUMLElementPair[Uml, Omf]]
  
  type RuleFunction =
  PartialFunction[
    ( MappingFunction[Uml, Omf],
      TboxUMLElementPair[Uml, Omf],
      UMLStereotype2EntityAspectMap,
      UMLStereotype2EntityConceptMap,
      UMLStereotype2EntityRelationshipMap,
      Set[UMLStereotype[Uml]] ),
    NonEmptyList[java.lang.Throwable] \/ ( TboxUMLElementPairs, TboxUMLElementPairs )]

  type Element2AspectCTorRuleFunction = Function3[
    MappingFunction[Uml, Omf],
    Omf#MutableModelTerminologyGraph,
    UMLClassifier[Uml],
    NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect]

  type Element2ConceptCTorRuleFunction = Function4[
    MappingFunction[Uml, Omf],
    Omf#MutableModelTerminologyGraph,
    UMLNamedElement[Uml],
    Boolean,
    NonEmptyList[java.lang.Throwable] \/ MappedEntityConcept]

  type Element2RelationshipCTorRuleFunction = Function8[
    MappingFunction[Uml, Omf],
    Omf#MutableModelTerminologyGraph,
    UMLNamedElement[Uml],
    Omf#ModelEntityDefinition,
    Omf#ModelEntityDefinition,
    Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    Boolean,
    Option[String],
    NonEmptyList[java.lang.Throwable] \/ MappedEntityRelationship]
  
  type MappedEntityConcept = Omf#ModelEntityConcept
  type MappedEntityRelationship = Omf#ModelEntityReifiedRelationship

  val abbrevName2Aspect = stereotype2Aspect map { case ( _, a ) =>
    toAbbreviatedName( fromEntityAspect( a ), false ).get -> a
  }

  val abbrevName2Concept = stereotype2Concept map { case ( _, c ) =>
    toAbbreviatedName( fromEntityConcept( c ).iri, false ).get -> c
  }

  val abbrevName2Relationship = stereotype2Relationship map { case ( _, r ) =>
    toAbbreviatedName( fromEntityReifiedRelationship( r ).iri, false ).get -> r
  }
  
  val abbrevName2Entity = abbrevName2Aspect ++ abbrevName2Concept ++ abbrevName2Relationship

  val allMappedStereotypes = stereotype2Aspect.keySet ++ stereotype2Concept.keySet ++ stereotype2Relationship.keySet

  def isStereotypeMapped( s: UMLStereotype[Uml] ): Boolean = allMappedStereotypes.contains( s )

  def isStereotypedElementMapped( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Boolean =
    e
    .getAppliedStereotypes
    .map{ m2p =>
      m2p
      .keys
      .exists( isStereotypeMapped )
    }

  def getAppliedStereotypesMappedToOMF
  ( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/
    ( UMLStereotype2EntityAspectMap,
      UMLStereotype2EntityConceptMap,
      UMLStereotype2EntityRelationshipMap,
      Set[UMLStereotype[Uml]] ) = {
    e
    .getAppliedStereotypes
    .map { m2p =>

      val appliedStereotypes =
        m2p
        .keySet
        .filter { s =>
          !ignoreCrossReferencedElementFilter(s)
        }

      (
        stereotype2Aspect.filterKeys(appliedStereotypes.contains),
        stereotype2Concept.filterKeys(appliedStereotypes.contains),
        stereotype2Relationship.filterKeys(appliedStereotypes.contains),
        appliedStereotypes -- allMappedStereotypes)
    }
  }

  def getSortedAppliedStereotypesMappedToOMF
  ( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/
    ( List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]] ) =
    getAppliedStereotypesMappedToOMF( e )
    .map { case (as, cs, rs, us) =>
      ( as.keys.toList.sortBy(_.qualifiedName.get),
        cs.keys.toList.sortBy(_.qualifiedName.get),
        rs.keys.toList.sortBy(_.qualifiedName.get),
        us.toList.sortBy(_.qualifiedName.get))
    }

  def doesStereotypedElementMap2Aspect( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Boolean =
    getAppliedStereotypesMappedToOMF( e )
    .map { case ( as, cs, _, _ ) =>
      as.nonEmpty && cs.isEmpty
    }

  def doesStereotypedElementMap2Concept( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Boolean =
    getAppliedStereotypesMappedToOMF( e )
    .map(_._2.nonEmpty)

  def doesStereotypedElementMap2AspectOrConcept( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Boolean =
  getAppliedStereotypesMappedToOMF( e )
  .map { case ( as, cs, _, _ ) =>
    as.nonEmpty || cs.nonEmpty
  }

  def doesStereotypedElementMap2Relationship( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Boolean =
    getAppliedStereotypesMappedToOMF( e )
    .map(_._3.nonEmpty)

  def partitionAppliedStereotypesByMapping
  ( e: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ ( Set[UMLStereotype[Uml]], Set[UMLStereotype[Uml]] ) =
    e
    .getAppliedStereotypes
    .map { m2p =>
      m2p
      .keySet
      .filter (s => ! ignoreCrossReferencedElementFilter (s) )
      .partition (isStereotypeMapped)
    }

  lazy val basePackageC = abbrevName2Concept( "base:Package" )
  lazy val baseContainsR = abbrevName2Relationship( "base:Contains" )

  val mappedElement2Aspect = scala.collection.mutable.HashMap[UMLElement[Uml], Omf#ModelEntityAspect]()
  def lookupElementAspectMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityAspect] =
    mappedElement2Aspect
      .get( e )

  def mapElement2Aspect
  ( rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect =
    element2aspectCtor.applyMapping( this, rule, tbox, u )

  val mappedElement2Concept = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityConcept]()

  def lookupElementConceptMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ModelEntityConcept] =
    mappedElement2Concept.get( e )

  def mapElement2Concept
  ( rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean)
  : NonEmptyList[java.lang.Throwable] \/ MappedEntityConcept =
    element2conceptCtor.applyMapping( this, rule, tbox, u, isAbstract )

  val mappedElement2Relationship = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityRelationship]()

  def lookupElementRelationshipMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ModelEntityReifiedRelationship] =
    mappedElement2Relationship.get( e )

  def mapElement2Relationship
  ( rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    hasName: Option[String] )
  : NonEmptyList[java.lang.Throwable] \/ MappedEntityRelationship =
    element2relationshipCtor.applyMapping(
      this, rule, tbox, u, source, target, characteristics, isAbstract, hasName )

  def lookupElementMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ModelEntityDefinition] =
    lookupElementAspectMapping( e ) orElse
      lookupElementConceptMapping( e ) orElse
      lookupElementRelationshipMapping( e )

  def getDependencySourceAndTargetMappings
  ( d: UMLDependency[Uml] )
  : Option[
    ( ( UMLNamedElement[Uml], Omf#ModelEntityDefinition ),
    ( UMLNamedElement[Uml], Omf#ModelEntityDefinition ) )] = {
    val sourceU = {
      require( d.client.size == 1 )
      d.client.head
    }
    val targetU = {
      require( d.supplier.size == 1 )
      d.supplier.head
    }

    for {
      sourceE <- lookupElementMapping(sourceU)
      targetE <- lookupElementMapping(targetU)
    } yield Tuple2( Tuple2( sourceU, sourceE ), Tuple2( targetU, targetE ) )
  }

  def getDirectedBinaryAssociationSourceAndTargetMappings
  ( a: UMLAssociation[Uml] )
  : Option[
    ( ( UMLClassifier[Uml], Omf#ModelEntityDefinition ),
      ( UMLClassifier[Uml], Omf#ModelEntityDefinition ) )] =
    for {

      directedEnds <- a.getDirectedAssociationEnd
      (sourcePU, targetPU) = directedEnds

      sourceTU <- sourcePU._type.selectByKindOf { case cls: UMLClassifier[Uml] => cls }
      sourceE <- lookupElementMapping(sourceTU)

      targetTU <- targetPU._type.selectByKindOf { case cls: UMLClassifier[Uml] => cls }
      targetE <- lookupElementMapping(targetTU)

    } yield
      Tuple2( Tuple2( sourceTU, sourceE ), Tuple2( targetTU, targetE ) )
}

case class OTI2OMFMapper[Uml <: UML, Omf <: OMF]() {

  /**
   * A rule result is a 3-tuple:
   * - the rule itself that produced the results
   * - a list of Tbox / UML Element pairs to be processed within this phase 
   * - a list of Tbox / UML Element pairs to be processed in the next phase
   */
  type RuleResult =
  ( MappingFunction[Uml, Omf],
    OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs,
    OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs )
  
  /**
   * Apply the first matching rule
   */
  def applyMatchingRule
  ( context: OTI2OMFMappingContext[Uml, Omf],
    current: TboxUMLElementPair[Uml, Omf],
    rules: List[MappingFunction[Uml, Omf]] )
  : NonEmptyList[java.lang.Throwable] \/ Option[RuleResult] =
    context.getAppliedStereotypesMappedToOMF( current.e )
    .flatMap { case ( as, cs, rs, us ) =>
      rules
      .dropWhile( ( r ) => !r.mappingRule.isDefinedAt( Tuple6( r, current, as, cs, rs, us ) ) ) match {
        case Nil =>
          Option.empty[RuleResult]
            .right
        case r :: _ =>
          r
          .mappingRule(Tuple6(r, current, as, cs, rs, us))
          .map { case (pairs1, pairs2) =>
            val result: RuleResult = (r, pairs1, pairs2)
            result.some
          }
      }
    }

  type RulesResult =
  ( OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs,
    OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs )
  
  /**
   * Successively apply all matching rules to each content pair until
   * there are no follow-on namespaces to apply to or none of the rules match.
   */
  def applyAllRules
  ( context: OTI2OMFMappingContext[Uml, Omf],
    tbox: Option[Omf#MutableModelTerminologyGraph],
    contentPairs: List[TboxUMLElementPair[Uml, Omf]],
    rules: List[MappingFunction[Uml, Omf]] )
  ( implicit omfOps: OMFOps[Omf] )
  : NonEmptyList[java.lang.Throwable] \/ RulesResult = {

    @annotation.tailrec def step
    ( queue: List[TboxUMLElementPair[Uml, Omf]],
      deferred: List[TboxUMLElementPair[Uml, Omf]],
      results: List[TboxUMLElementPair[Uml, Omf]] )
    : NonEmptyList[java.lang.Throwable] \/ RulesResult =
      queue match {
        case Nil =>
          \/-( ( deferred, results ) )
        case pair :: pairs =>
          applyMatchingRule( context, pair, rules ) match {
            case -\/( f ) =>
              -\/( f )
            case \/-( None ) =>
              step( pairs, pair :: deferred, results )
            case \/-( Some( ( rule, morePairs, moreResults ) ) ) =>
              step( morePairs ::: pairs, deferred, moreResults ::: results )
          }
      }

    step( contentPairs, Nil, Nil )
  }

}