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
    context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] ): Omf#ModelEntityAspect
}

trait Element2AspectCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2AspectCTor[Uml, Omf]
  with OTI2OMFMappingContext[Uml, Omf]#Element2AspectCTorRuleFunction {

  override def applyMapping(
    context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] ): Omf#ModelEntityAspect = {

    val aspect = apply( rule, tbox, u )
    context.mappedElement2Aspect += ( u -> aspect )
    aspect
  }

}

trait Element2ConceptCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping(
    context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean ): OTI2OMFMappingContext[Uml, Omf]#MappedEntityConcept
}

trait Element2ConceptCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2ConceptCTor[Uml, Omf]
  with OTI2OMFMappingContext[Uml, Omf]#Element2ConceptCTorRuleFunction {

  override def applyMapping(
    context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean ): OTI2OMFMappingContext[Uml, Omf]#MappedEntityConcept = {
    val conceptGraph = apply( rule, tbox, u, isAbstract )
    context.mappedElement2Concept += ( u -> conceptGraph )
    conceptGraph
  }

}

trait Element2RelationshipCTor[Uml <: UML, Omf <: OMF] {
  def applyMapping(
    context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: String,
    qualifiedName: String ): OTI2OMFMappingContext[Uml, Omf]#MappedEntityRelationship
}

trait Element2RelationshipCTorFunction[Uml <: UML, Omf <: OMF]
  extends Element2RelationshipCTor[Uml, Omf]
  with OTI2OMFMappingContext[Uml, Omf]#Element2RelationshipCTorRuleFunction {

  override def applyMapping(
    context: OTI2OMFMappingContext[Uml, Omf],
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: String,
    qualifiedName: String ): OTI2OMFMappingContext[Uml, Omf]#MappedEntityRelationship = {

    val relationshipGraph = apply( rule, tbox, u, source, target, characteristics, isAbstract, name, qualifiedName )
    context.mappedElement2Relationship += ( u -> relationshipGraph )
    relationshipGraph
  }
}

case class TboxUMLElementPair[Uml <: UML, Omf <: OMF](
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
  val mappingRule: OTI2OMFMappingContext[Uml, Omf]#RuleFunction )( implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] )

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

  protected val element2aspectCtor: Element2AspectCTor[Uml, Omf],
  protected val element2conceptCtor: Element2ConceptCTor[Uml, Omf],
  protected val element2relationshipCtor: Element2RelationshipCTor[Uml, Omf],

  addEntityDefinitionAspectSubClassAxiom: AddEntityDefinitionAspectSubClassAxiom[Uml, Omf],
  addEntityConceptSubClassAxiom: AddEntityConceptSubClassAxiom[Uml, Omf],
  addEntityRelationshipSubClassAxiom: AddEntityRelationshipSubClassAxiom[Uml, Omf],

  stereotype2Aspect: Map[UMLStereotype[Uml], Omf#ModelEntityAspect],
  stereotype2Concept: Map[UMLStereotype[Uml], Omf#ModelEntityConcept],
  stereotype2Relationship: Map[UMLStereotype[Uml], Omf#ModelEntityRelationship],
  otherStereotypesApplied: Set[UMLStereotype[Uml]],
  ops: OMFOps[Omf] ) {

  import ops._

  type UMLStereotype2EntityAspectMap = Map[UMLStereotype[Uml], Omf#ModelEntityAspect]
  type UMLStereotype2EntityConceptMap = Map[UMLStereotype[Uml], Omf#ModelEntityConcept]
  type UMLStereotype2EntityRelationshipMap = Map[UMLStereotype[Uml], Omf#ModelEntityRelationship]
  type TboxUMLElementPairs = List[TboxUMLElementPair[Uml, Omf]]
  
  type RuleFunction = PartialFunction[( MappingFunction[Uml, Omf], TboxUMLElementPair[Uml, Omf], UMLStereotype2EntityAspectMap, UMLStereotype2EntityConceptMap, UMLStereotype2EntityRelationshipMap, Set[UMLStereotype[Uml]] ), Try[( TboxUMLElementPairs, TboxUMLElementPairs )]]
  type Element2AspectCTorRuleFunction = Function3[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, UMLClassifier[Uml], Omf#ModelEntityAspect]
  type Element2ConceptCTorRuleFunction = Function4[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, UMLNamedElement[Uml], Boolean, MappedEntityConcept]
  type Element2RelationshipCTorRuleFunction = Function9[MappingFunction[Uml, Omf], Omf#MutableModelTerminologyGraph, UMLNamedElement[Uml], Omf#ModelEntityDefinition, Omf#ModelEntityDefinition, Iterable[RelationshipCharacteristics.RelationshipCharacteristics], Boolean, String, String, MappedEntityRelationship]
  
  type MappedEntityConcept = ( Omf#ModelEntityConcept, Option[Omf#MutableModelTerminologyGraph] )
  type MappedEntityRelationship = ( Omf#ModelEntityRelationship, Option[Omf#MutableModelTerminologyGraph] )

  val abbrevName2Aspect = stereotype2Aspect map { case ( _, a ) => ( toAbbreviatedName( fromEntityAspect( a ), false ).get -> a ) } toMap;
  val abbrevName2Concept = stereotype2Concept map { case ( _, c ) => ( toAbbreviatedName( fromEntityConcept( c )._1, false ).get -> c ) } toMap;
  val abbrevName2Relationship = stereotype2Relationship map { case ( _, r ) => ( toAbbreviatedName( fromEntityRelationship( r )._1, false ).get -> r ) } toMap;

  val abbrevName2Entity = abbrevName2Aspect ++ abbrevName2Concept ++ abbrevName2Relationship

  val allMappedStereotypes = stereotype2Aspect.keySet ++ stereotype2Concept.keySet ++ stereotype2Relationship.keySet

  def isStereotypeMapped( s: UMLStereotype[Uml] ): Boolean = allMappedStereotypes.contains( s )

  def isStereotypedElementMapped( e: UMLElement[Uml] ): Boolean =
    e.getAppliedStereotypes.keys.exists( isStereotypeMapped( _ ) )

  def getAppliedStereotypesMappedToOMF( e: UMLElement[Uml] ): ( UMLStereotype2EntityAspectMap, UMLStereotype2EntityConceptMap, UMLStereotype2EntityRelationshipMap, Set[UMLStereotype[Uml]] ) = {
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
      as.keys.toList.sortBy( _.qualifiedName.get ),
      cs.keys.toList.sortBy( _.qualifiedName.get ),
      rs.keys.toList.sortBy( _.qualifiedName.get ),
      us.toList.sortBy( _.qualifiedName.get ) )
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
  lazy val baseContainsR = abbrevName2Relationship( "base:Contains" )

  val mappedElement2Aspect = scala.collection.mutable.HashMap[UMLElement[Uml], Omf#ModelEntityAspect]()
  def lookupElementAspectMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityAspect] =
    mappedElement2Aspect.get( e ) match {
      case None      => None
      case Some( a ) => Some( a )
    }
  def mapElement2Aspect(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLClassifier[Uml] ): Omf#ModelEntityAspect =
    element2aspectCtor.applyMapping( this, rule, tbox, u )

  val mappedElement2Concept = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityConcept]()
  def lookupElementConceptMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityConcept] =
    mappedElement2Concept.get( e ) match {
      case None             => None
      case Some( ( c, _ ) ) => Some( c )
    }
  def mapElement2Concept(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean ): MappedEntityConcept =
    element2conceptCtor.applyMapping( this, rule, tbox, u, isAbstract )

  val mappedElement2Relationship = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityRelationship]()
  def lookupElementRelationshipMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityRelationship] =
    mappedElement2Relationship.get( e ) match {
      case None             => None
      case Some( ( r, _ ) ) => Some( r )
    }
  def mapElement2Relationship(
    rule: MappingFunction[Uml, Omf],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: String,
    qualifiedName: String ): MappedEntityRelationship =
    element2relationshipCtor.applyMapping( this, rule, tbox, u, source, target, characteristics, isAbstract, name, qualifiedName )

  def lookupElementMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityDefinition] =
    lookupElementAspectMapping( e ) orElse lookupElementConceptMapping( e ) orElse lookupElementRelationshipMapping( e )

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

  def getDirectedBinaryAssociationSourceAndTargetMappings( a: UMLAssociation[Uml] ): Option[( ( UMLClassifier[Uml], Omf#ModelEntityDefinition ), ( UMLClassifier[Uml], Omf#ModelEntityDefinition ) )] =
    a.getDirectedAssociationEnd match {
      case None => None
      case Some( ( sourcePU, targetPU ) ) =>
        ( sourcePU._type, targetPU._type ) match {
          case ( Some( sourceTU: UMLClassifier[Uml] ), Some( targetTU: UMLClassifier[Uml] ) ) =>
            ( lookupElementMapping( sourceTU ), lookupElementMapping( targetTU ) ) match {
              case ( Some( sourceE ), Some( targetE ) ) => Some( ( sourceTU, sourceE ), ( targetTU, targetE ) )
              case ( _, _ )                             => None
            }
          case ( _, _ ) => None
        }
    }
}

case class OTI2OMFMapper[Uml <: UML, Omf <: OMF]() {

  /**
   * A rule result is a 3-tuple:
   * - the rule itself that produced the results
   * - a list of Tbox / UML Element pairs to be processed within this phase 
   * - a list of Tbox / UML Element pairs to be processed in the next phase
   */
  type RuleResult = ( MappingFunction[Uml, Omf], OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs, OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs )
  
  /**
   * Apply the first matching rule
   */
  def applyMatchingRule(
    context: OTI2OMFMappingContext[Uml, Omf],
    current: TboxUMLElementPair[Uml, Omf],
    rules: List[MappingFunction[Uml, Omf]] ): Try[Option[RuleResult]] = {
    val ( as, cs, rs, us ) = context.getAppliedStereotypesMappedToOMF( current.ns )
    rules.dropWhile( ( r ) => !r.mappingRule.isDefinedAt( r, current, as, cs, rs, us ) ) match {
      case Nil =>
        Success( None )
      case r :: _ =>
        r.mappingRule( r, current, as, cs, rs, us ) match {
          case Failure( t )                  => Failure( t )
          case Success( ( pairs1, pairs2 ) ) => Success( Some( r, pairs1, pairs2 ) )
        }
    }
  }

  type RulesResult = ( OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs, OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs )
  
  /**
   * Successively apply all matching rules to each content pair until there are no follow-on namespaces to apply to or none of the rules match.
   */
  def applyAllRules(
    context: OTI2OMFMappingContext[Uml, Omf],
    tbox: Option[Omf#MutableModelTerminologyGraph],
    contentPairs: List[TboxUMLElementPair[Uml, Omf]],
    rules: List[MappingFunction[Uml, Omf]] )( implicit omfOps: OMFOps[Omf] ): Try[RulesResult] = {

    @annotation.tailrec def step(
      queue: List[TboxUMLElementPair[Uml, Omf]],
      deferred: List[TboxUMLElementPair[Uml, Omf]],
      results: List[TboxUMLElementPair[Uml, Omf]] ): Try[RulesResult] =
      queue match {
        case Nil => Success( ( deferred, results ) )
        case pair :: pairs =>
          applyMatchingRule( context, pair, rules ) match {
            case Failure( t ) => Failure( t )
            case Success( None ) =>
              step( pairs, pair :: deferred, results )
            case Success( Some( ( rule, morePairs, moreResults ) ) ) =>
              step( morePairs ::: pairs, deferred, moreResults ::: results )
          }
      }

    step( contentPairs, Nil, Nil )
  }

}