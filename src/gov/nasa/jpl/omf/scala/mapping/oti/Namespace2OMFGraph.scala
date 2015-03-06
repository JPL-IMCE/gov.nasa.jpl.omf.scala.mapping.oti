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
    u: UMLClassifier[Uml] ): Omf#ModelEntityAspect = {

    val aspect = apply( rule, tbox, u )
    rule.context.mappedElement2Aspect += ( u -> aspect )
    aspect
  }

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
    isAbstract: Boolean ): ( Omf#ModelEntityConcept, Option[Omf#MutableModelTerminologyGraph] ) = {
    val conceptGraph = apply( rule, tbox, u, isAbstract )
    rule.context.mappedElement2Concept += ( u -> conceptGraph )
    conceptGraph
  }

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
    qualifiedName: String ): ( Omf#ModelEntityRelationship, Option[Omf#MutableModelTerminologyGraph] ) = {

    val relationshipGraph = apply( rule, tbox, u, source, target, characteristics, isAbstract, name, qualifiedName )
    rule.context.mappedElement2Relationship += ( u -> relationshipGraph )
    relationshipGraph
  }
}

case class TboxNestedNamespacePair[Uml <: UML, Omf <: OMF](
  tbox: Option[Omf#MutableModelTerminologyGraph],
  ns: UMLElement[Uml] )( implicit omfOps: OMFOps[Omf] ) {
  override def toString: String =
    tbox match {
      case None =>
        s"[tbox=<none>, ${ns.xmiType.head}: ${ns.xmiID.head}]"
      case Some( g ) =>
        s"[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${ns.xmiType.head}: ${ns.xmiID.head}]"
    }
}

//case class TboxContentPair[Uml <: UML, Omf <: OMF](
//  tbox: Omf#MutableModelTerminologyGraph,
//  e: UMLElement[Uml] )

case class MappingFunction[Uml <: UML, Omf <: OMF](
  val name: String,
  val context: OTI2OMFMappingContext[Uml, Omf],
  val mappingRule: PartialFunction[( MappingFunction[Uml, Omf], TboxNestedNamespacePair[Uml, Omf], Map[UMLStereotype[Uml], Omf#ModelEntityAspect], Map[UMLStereotype[Uml], Omf#ModelEntityConcept], Map[UMLStereotype[Uml], Omf#ModelEntityRelationship], Set[UMLStereotype[Uml]] ), Try[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxNestedNamespacePair[Uml, Omf]] )]] )(
    implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] )

trait Namespace2TBoxCtor[Uml <: UML, Omf <: OMF]
  extends Function2[MappingFunction[Uml, Omf], UMLNamespace[Uml], Omf#MutableModelTerminologyGraph]

trait AddEntityDefinitionAspectSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, Omf#ModelEntityDefinition, Omf#ModelEntityAspect, Omf#EntityDefinitionAspectSubClassAxiom]

trait AddEntityConceptSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, Omf#ModelEntityConcept, Omf#ModelEntityConcept, Omf#EntityConceptSubClassAxiom]

trait AddEntityRelationshipSubClassAxiom[Uml <: UML, Omf <: OMF]
  extends Function4[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, Omf#ModelEntityRelationship, Omf#ModelEntityRelationship, Omf#EntityRelationshipSubClassAxiom]

case class OTI2OMFMappingContext[Uml <: UML, Omf <: OMF](
  iriPrefix: String,
  tboxLookup: Namespace2TBoxLookupFunction[Uml, Omf],
  ns2tboxCtor: Namespace2TBoxCtor[Uml, Omf],

  element2aspectCtor: Element2AspectCTor[Uml, Omf],
  element2conceptCtor: Element2ConceptCTor[Uml, Omf],
  element2relationshipCtor: Element2RelationshipCTor[Uml, Omf],

  addEntityDefinitionAspectSubClassAxiom: AddEntityDefinitionAspectSubClassAxiom[Uml, Omf],
  addEntityConceptSubClassAxiom: AddEntityConceptSubClassAxiom[Uml, Omf],
  addEntityRelationshipSubClassAxiom: AddEntityRelationshipSubClassAxiom[Uml, Omf],

  stereotype2Aspect: Map[UMLStereotype[Uml], Omf#ModelEntityAspect],
  stereotype2Concept: Map[UMLStereotype[Uml], Omf#ModelEntityConcept],
  stereotype2Relationship: Map[UMLStereotype[Uml], Omf#ModelEntityRelationship],
  ops: OMFOps[Omf] ) {

  import ops._

  type RuleFunction = PartialFunction[( MappingFunction[Uml, Omf], TboxNestedNamespacePair[Uml, Omf], Map[UMLStereotype[Uml], Omf#ModelEntityAspect], Map[UMLStereotype[Uml], Omf#ModelEntityConcept], Map[UMLStereotype[Uml], Omf#ModelEntityRelationship], Set[UMLStereotype[Uml]] ), Try[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxNestedNamespacePair[Uml, Omf]] )]]

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

  def getSortedAppliedStereotypesMappedToOMF( e: UMLElement[Uml] ): ( List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]] ) = {
    val ( as, cs, rs, us ) = getAppliedStereotypesMappedToOMF( e )
    ( 
      as.keys.toList.sortBy(_.qualifiedName.get),
      cs.keys.toList.sortBy(_.qualifiedName.get),
      rs.keys.toList.sortBy(_.qualifiedName.get),
      us.toList.sortBy(_.qualifiedName.get) )
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

  val mappedElement2Aspect = scala.collection.mutable.HashMap[UMLElement[Uml], Omf#ModelEntityAspect]()

  val mappedElement2Concept = scala.collection.mutable.HashMap[UMLElement[Uml], ( Omf#ModelEntityConcept, Option[Omf#MutableModelTerminologyGraph] )]()
  def lookupElementConceptMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityConcept] =
    mappedElement2Concept.get( e ) match {
      case None             => None
      case Some( ( c, _ ) ) => Some( c )
    }

  val mappedElement2Relationship = scala.collection.mutable.HashMap[UMLElement[Uml], ( Omf#ModelEntityRelationship, Option[Omf#MutableModelTerminologyGraph] )]()
  def lookupElementRelationshipMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityRelationship] =
    mappedElement2Relationship.get( e ) match {
      case None             => None
      case Some( ( r, _ ) ) => Some( r )
    }

  def lookupElementMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityDefinition] =
    mappedElement2Aspect.get( e ) orElse lookupElementConceptMapping( e ) orElse lookupElementRelationshipMapping( e )

  def getDependencySourceAndTargetMappings( d: UMLDependency[Uml] ): Option[( ( UMLNamedElement[Uml], Omf#ModelEntityDefinition ), ( UMLNamedElement[Uml], Omf#ModelEntityDefinition ) )] = {
    val sourceU = {
      require( d.client.size == 1 )
      d.client.head
    }
    val targetU = {
      require( d.supplier.size == 1 )
      d.supplier.head
    }

    ( lookupElementMapping( sourceU ), lookupElementMapping( targetU ) ) match {
      case ( Some( sourceE ), Some( targetE ) ) => Some( ( sourceU, sourceE ), ( targetU, targetE ) )
      case ( _, _ )                             => None
    }

  }
}

object Namespace2OMFGraph {

  /**
   * Successively apply all matching rules until there are no follow-on namespaces to apply to or none of the rules match.
   */
  def applyAllRules[Uml <: UML, Omf <: OMF](
    context: OTI2OMFMappingContext[Uml, Omf],
    tbox: Option[Omf#MutableModelTerminologyGraph],
    ns: UMLNamespace[Uml],
    rules: List[MappingFunction[Uml, Omf]] )( implicit omfOps: OMFOps[Omf] ): Try[List[TboxNestedNamespacePair[Uml, Omf]]] = {

    @annotation.tailrec def step(
      queue: List[TboxNestedNamespacePair[Uml, Omf]],
      contents: List[TboxNestedNamespacePair[Uml, Omf]] ): Try[List[TboxNestedNamespacePair[Uml, Omf]]] =
      queue match {
        case Nil => Success( contents )
        case pair :: pairs =>
          applyMatchingRule( context, pair, rules ) match {
            case Failure( t )                           => Failure( t )
            case Success( None ) =>
              System.out.println(s"\n* No match for: ${ns.qualifiedName.get}")
              step( pairs, contents )
            case Success( Some( ( morePairs, moreContents ) ) ) => 
              step( morePairs ::: pairs, moreContents ::: contents )
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
    rules: List[MappingFunction[Uml, Omf]] ): Try[Option[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxNestedNamespacePair[Uml, Omf]] )]] = {
    val ( as, cs, rs, us ) = context.getAppliedStereotypesMappedToOMF( current.ns )
    rules.dropWhile( ( r ) => !r.mappingRule.isDefinedAt( r, current, as, cs, rs, us ) ) match {
      case Nil =>
        Success( None )
      case r :: _ =>
        r.mappingRule( r, current, as, cs, rs, us ) match {
          case Failure( t ) => Failure( t )
          case Success( ( pairs1, pairs2 ) ) => Success( Some( pairs1, pairs2 ) )
        }
    }
  }

  def mapAllContentPairs[Uml <: UML, Omf <: OMF](
    context: OTI2OMFMappingContext[Uml, Omf],
    tbox: Option[Omf#MutableModelTerminologyGraph],
    contentPairs: List[TboxNestedNamespacePair[Uml, Omf]],
    rules: List[MappingFunction[Uml, Omf]] ): Try[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxNestedNamespacePair[Uml, Omf]] )] = {

    @annotation.tailrec def step(
      queue: List[TboxNestedNamespacePair[Uml, Omf]],
      deferred: List[TboxNestedNamespacePair[Uml, Omf]],
      results: List[TboxNestedNamespacePair[Uml, Omf]] ): Try[( List[TboxNestedNamespacePair[Uml, Omf]], List[TboxNestedNamespacePair[Uml, Omf]] )] =
      queue match {
        case Nil => Success( ( deferred, results ) )
        case pair :: pairs =>
          applyMatchingRule( context, pair, rules ) match {
            case Failure( t )                          => Failure( t )
            case Success( None ) =>
              System.out.println(s"\n* No match for: ${pair.ns.xmiType.head}: ${pair.ns.id}")
              step( pairs, pair :: deferred, results )
            case Success( Some( ( morePairs, moreResults ) ) ) => 
              step( morePairs ::: pairs, deferred, moreResults ::: results )
          }
      }

    step( contentPairs, Nil, Nil )
  }
}