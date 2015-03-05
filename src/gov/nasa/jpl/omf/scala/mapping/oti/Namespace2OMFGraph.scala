package gov.nasa.jpl.omf.scala.mapping.oti

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.omg.oti._
import org.omg.oti.api._
import org.omg.oti.operations._
import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.reflect.runtime.universe._

object Namespace2OMFTypeTermKind extends Enumeration {
  type Namespace2OMFTypeTermKind = Value
  val Aspect = Value
  val Concept = Value
  val Relationship = Value
}

trait Namespace2TBoxLookupFunction[Uml <: UML, Omf <: OMF]
  extends Function1[UMLNamespace[Uml], Option[Omf#ModelTerminologyGraph]]

trait Element2AspectCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] ): Omf#ModelEntityAspect
}

trait Element2AspectCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2AspectCTor[Uml, Omf]
  with Function3[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, UMLClassifier[Uml], Omf#ModelEntityAspect] {

  override def applyMapping(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] ): Omf#ModelEntityAspect =
    apply( rule, tbox, u )

}

trait Element2ConceptCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean ): ( Omf#ModelEntityConcept, Option[Omf#MutableModelTerminologyGraph] )
}

trait Element2ConceptCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2ConceptCTor[Uml, Omf]
  with Function4[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, UMLNamedElement[Uml], Boolean, ( Omf#ModelEntityConcept, Option[Omf#MutableModelTerminologyGraph] )] {

  override def applyMapping(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean ): ( Omf#ModelEntityConcept, Option[Omf#MutableModelTerminologyGraph] ) =
    apply( rule, tbox, u, isAbstract )

}

trait Element2RelationshipCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: String,
    qualifiedName: String ): ( Omf#ModelEntityRelationship, Option[Omf#MutableModelTerminologyGraph] )
}

trait Element2RelationshipCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2RelationshipCTor[Uml, Omf]
  with Function9[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, UMLNamedElement[Uml], Omf#ModelEntityDefinition, Omf#ModelEntityDefinition, Iterable[RelationshipCharacteristics.RelationshipCharacteristics], Boolean, String, String, ( Omf#ModelEntityRelationship, Option[Omf#MutableModelTerminologyGraph] )] {

  override def applyMapping(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: String,
    qualifiedName: String ): ( Omf#ModelEntityRelationship, Option[Omf#MutableModelTerminologyGraph] ) =
    apply( rule, tbox, u, source, target, characteristics, isAbstract, name, qualifiedName )
}

case class TboxNestedNamespacePair[Uml <: UML, Omf <: OMF](
  tbox: Option[Omf#MutableModelTerminologyGraph],
  ns: UMLNamespace[Uml] )( implicit omfOps: OMFOps[Omf] ) {
  override def toString: String =
    tbox match {
      case None =>
        s"[tbox=<none>, ${ns.xmiType.head}: ${ns.qualifiedName.get}]"
      case Some( g ) =>
        s"[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${ns.xmiType.head}: ${ns.qualifiedName.get}]"
    }
}

case class TboxContentPair[Uml <: UML, Omf <: OMF](
  tbox: Omf#MutableModelTerminologyGraph,
  e: UMLElement[Uml] )

case class MappingFunction[Uml <: UML, Omf <: OMF](
  val name: String,
  val context: OTI2OMFMappingContext[Uml, Omf],
  val mappingRule: PartialFunction[( MappingFunction[Uml, Omf], TboxNestedNamespacePair[Uml, Omf], Map[UMLStereotype[Uml], Omf#ModelEntityAspect], Map[UMLStereotype[Uml], Omf#ModelEntityConcept], Map[UMLStereotype[Uml], Omf#ModelEntityRelationship], Set[UMLStereotype[Uml]] ), Try[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxContentPair[Uml, Omf]] )]] )(
    implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] )

trait Namespace2TBoxCtor[Uml <: UML, Omf <: OMF]
  extends Function2[MappingFunction[Uml, Omf], UMLNamespace[Uml], Omf#MutableModelTerminologyGraph]

trait AddEntityDefinitionAspectSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, Omf#ModelEntityDefinition, Omf#ModelEntityAspect, Omf#EntityDefinitionAspectSubClassAxiom]

trait AddEntityConceptSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, Omf#ModelEntityConcept, Omf#ModelEntityConcept, Omf#EntityConceptSubClassAxiom]

case class OTI2OMFMappingContext[Uml <: UML, Omf <: OMF](
  iriPrefix: String,
  tboxLookup: Namespace2TBoxLookupFunction[Uml, Omf],
  ns2tboxCtor: Namespace2TBoxCtor[Uml, Omf],
  
  element2aspectCtor: Element2AspectCTor[Uml, Omf],
  element2conceptCtor: Element2ConceptCTor[Uml, Omf],
  element2relationshipCtor: Element2RelationshipCTor[Uml, Omf],
  
  addEntityDefinitionAspectSubClassAxiom: AddEntityDefinitionAspectSubClassAxiom[Uml, Omf],
  addEntityConceptSubClassAxiom: AddEntityConceptSubClassAxiom[Uml, Omf],
  
  stereotype2Aspect: Map[UMLStereotype[Uml], Omf#ModelEntityAspect],
  stereotype2Concept: Map[UMLStereotype[Uml], Omf#ModelEntityConcept],
  stereotype2Relationship: Map[UMLStereotype[Uml], Omf#ModelEntityRelationship] )( implicit ops: OMFOps[Omf] ) {

  import ops._

  type RuleFunction = PartialFunction[( MappingFunction[Uml, Omf], TboxNestedNamespacePair[Uml, Omf], Map[UMLStereotype[Uml], Omf#ModelEntityAspect], Map[UMLStereotype[Uml], Omf#ModelEntityConcept], Map[UMLStereotype[Uml], Omf#ModelEntityRelationship], Set[UMLStereotype[Uml]] ), Try[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxContentPair[Uml, Omf]] )]]

  val abbrevName2Aspect = stereotype2Aspect map { case ( _, a ) => ( toAbbreviatedName( fromEntityAspect( a ), false ).get -> a ) } toMap;
  val abbrevName2Concept = stereotype2Concept map { case ( _, c ) => ( toAbbreviatedName( fromEntityConcept( c )._1, false ).get -> c ) } toMap;
  val abbrevName2Relationship = stereotype2Relationship map { case ( _, r ) => ( toAbbreviatedName( fromEntityRelationship( r )._1, false ).get -> r ) } toMap;

  val abbrevName2Entity = abbrevName2Aspect ++ abbrevName2Concept ++ abbrevName2Relationship

  val allMappedStereotypes = stereotype2Aspect.keySet ++ stereotype2Concept.keySet ++ stereotype2Relationship.keySet

  def isStereotypeMapped( s: UMLStereotype[Uml] ): Boolean = allMappedStereotypes.contains( s )

  def isStereotypedElementMapped( e: UMLElement[Uml] ): Boolean =
    e.getAppliedStereotypes.keys.exists( isStereotypeMapped( _ ) )

  def getAppliedStereotypesMappedToOMF( e: UMLElement[Uml] ): ( Map[UMLStereotype[Uml], Omf#ModelEntityAspect], Map[UMLStereotype[Uml], Omf#ModelEntityConcept], Map[UMLStereotype[Uml], Omf#ModelEntityRelationship], Set[UMLStereotype[Uml]] ) = {
    val appliedStereotypes = e.getAppliedStereotypes.keySet
    (
      stereotype2Aspect.filterKeys( appliedStereotypes.contains( _ ) ),
      stereotype2Concept.filterKeys( appliedStereotypes.contains( _ ) ),
      stereotype2Relationship.filterKeys( appliedStereotypes.contains( _ ) ),
      appliedStereotypes -- allMappedStereotypes )
  }

  def doesStereotypedElementMap2Aspect( e: UMLElement[Uml] ): Boolean = {
    val ( as, cs, _, _ ) = getAppliedStereotypesMappedToOMF( e )
    as.nonEmpty && cs.isEmpty
  }

  def doesStereotypedElementMap2Concept( e: UMLElement[Uml] ): Boolean =
    getAppliedStereotypesMappedToOMF( e )._2.nonEmpty

  def doesStereotypedElementMap2AspectOrConcept( e: UMLElement[Uml] ): Boolean = {
    val ( as, cs, _, _ ) = getAppliedStereotypesMappedToOMF( e )
    as.nonEmpty || cs.nonEmpty
  }

  def doesStereotypedElementMap2Relationship( e: UMLElement[Uml] ): Boolean =
    getAppliedStereotypesMappedToOMF( e )._3.nonEmpty

  def partitionAppliedStereotypesByMapping( e: UMLElement[Uml] ): ( Set[UMLStereotype[Uml]], Set[UMLStereotype[Uml]] ) =
    e.getAppliedStereotypes.keySet.partition( isStereotypeMapped( _ ) )

  lazy val basePackageC = abbrevName2Concept( "base:Package" )
}

object Namespace2OMFGraph {

  /**
   * Successively apply all matching rules until there are no follow-on namespaces to apply to or none of the rules match.
   */
  def applyAllRules[Uml <: UML, Omf <: OMF](
    context: OTI2OMFMappingContext[Uml, Omf],
    tbox: Option[Omf#MutableModelTerminologyGraph],
    ns: UMLNamespace[Uml],
    rules: List[MappingFunction[Uml, Omf]] )( implicit omfOps: OMFOps[Omf] ): Try[List[TboxContentPair[Uml, Omf]]] = {

    @annotation.tailrec def step(
      queue: List[TboxNestedNamespacePair[Uml, Omf]],
      contents: List[TboxContentPair[Uml, Omf]] ): Try[List[TboxContentPair[Uml, Omf]]] =
      queue match {
        case Nil => Success( contents )
        case pair :: pairs =>
          applyMatchingRule( context, pair, rules ) match {
            case Failure( t )                           => Failure( t )
            case Success( ( morePairs, moreContents ) ) => step( morePairs ::: pairs, moreContents ::: contents )
          }
      }

    step( TboxNestedNamespacePair[Uml, Omf]( tbox, ns ) :: Nil, Nil )
  }

  /**
   * Apply the first matching rule
   */
  def applyMatchingRule[Uml <: UML, Omf <: OMF](
    context: OTI2OMFMappingContext[Uml, Omf],
    current: TboxNestedNamespacePair[Uml, Omf],
    rules: List[MappingFunction[Uml, Omf]] ): Try[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxContentPair[Uml, Omf]] )] = {
    val ( as, cs, rs, us ) = context.getAppliedStereotypesMappedToOMF( current.ns )
    rules.dropWhile( ( r ) => !r.mappingRule.isDefinedAt( r, current, as, cs, rs, us ) ) match {
      case Nil =>
        System.out.println( "\n*** no rule for: "+current )
        Success( Nil, Nil )
      case r :: _ =>
        r.mappingRule( r, current, as, cs, rs, us )
    }
  }

}