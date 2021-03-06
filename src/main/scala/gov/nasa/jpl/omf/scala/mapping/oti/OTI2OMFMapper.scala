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

package gov.nasa.jpl.omf.scala.mapping.oti

import gov.nasa.jpl.imce.oml.provenance.oti.{OML2OTIProvenance, OML2OTIProvenanceTables}
import gov.nasa.jpl.omf.scala.core._
import org.omg.oti.json.common.OTIPrimitiveTypes
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.canonicalXMI.ResolvedDocumentSet
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.trees._
import org.omg.oti.uml.xmi._

import scala.Predef.{Map => _, Set => _, _}
import scala.collection.immutable._
import scala.collection.parallel._
import scala.{Boolean, Enumeration, Function1, Function2, Function3, Function4, Function5, Function6, Function7, annotation}
import scala.{None, Option, PartialFunction, Some, StringContext, Tuple2, Tuple6, Unit}
import scalaz._
import Scalaz._

object Namespace2OMFTypeTermKind extends Enumeration {
  type Namespace2OMFTypeTermKind = Value
  val Aspect = Value
  val Concept = Value
  val Relationship = Value
}

trait Namespace2TBoxLookupFunction[Uml <: UML, Omf <: OMF]
  extends Function2[UMLNamespace[Uml], TerminologyKind, Option[Omf#TerminologyBox]]

trait Element2AspectCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Omf#Aspect
}

trait Element2AspectCTorFunction[Uml <: UML, Omf <: OMF, Provenance]
  extends Element2AspectCTor[Uml, Omf, Provenance]
  with OTI2OMFMappingContext[Uml, Omf, Provenance]#Element2AspectCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Omf#Aspect
  = apply(rule, tbox, u)

}

trait Element2ConceptCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLNamedElement[Uml])
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept
}

trait Element2ConceptCTorFunction[Uml <: UML, Omf <: OMF, Provenance]
  extends Element2ConceptCTor[Uml, Omf, Provenance]
  with OTI2OMFMappingContext[Uml, Omf, Provenance]#Element2ConceptCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLNamedElement[Uml])
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept
  = apply(rule, tbox, u)

}

trait Element2RelationshipCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLNamedElement[Uml],
    source: Omf#Entity,
    target: Omf#Entity,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    name: Option[String] )
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityRelationship
}

trait Element2RelationshipCTorFunction[Uml <: UML, Omf <: OMF, Provenance]
  extends Element2RelationshipCTor[Uml, Omf, Provenance]
  with OTI2OMFMappingContext[Uml, Omf, Provenance]#Element2RelationshipCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLNamedElement[Uml],
    source: Omf#Entity,
    target: Omf#Entity,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    name: Option[String] )
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityRelationship
  = apply(rule, tbox, u, source, target, characteristics, name)

}

case class MappingFunction[Uml <: UML, Omf <: OMF, Provenance]
( name: String,
  mappingRule: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction )
( implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] )

trait Namespace2TBoxCtor[Uml <: UML, Omf <: OMF, Provenance]
  extends Function4[
    MappingFunction[Uml, Omf, Provenance],
    UMLNamespace[Uml],
    TerminologyKind,
    Provenance,
    Set[java.lang.Throwable] \/ Omf#MutableTerminologyBox]

trait AddDirectlyExtendedTerminologyGraph[Uml <: UML, Omf <: OMF, Provenance]
  extends Function3[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    Omf#TerminologyBox,
    Set[java.lang.Throwable] \/ Omf#TerminologyExtensionAxiom]

trait AddDirectlyNestedTerminologyGraph[Uml <: UML, Omf <: OMF, Provenance]
  extends Function4[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    Omf#Concept,
    Omf#MutableTerminologyBox,
    Set[java.lang.Throwable] \/ Omf#TerminologyNestingAxiom]

trait AddEntityDefinitionAspectSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function6[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLElement[Uml],
    Omf#Entity,
    UMLStereotype[Uml],
    Omf#Aspect,
    Set[java.lang.Throwable] \/ Omf#AspectSpecializationAxiom]

trait AddEntityConceptSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function6[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLNamedElement[Uml],
    Omf#Concept,
    UMLStereotype[Uml],
    Omf#Concept,
    Set[java.lang.Throwable] \/ Omf#ConceptSpecializationAxiom]

trait AddEntityDefinitionExistentialRestrictionAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function7[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLElement[Uml],
    UMLStereotype[Uml],
    Omf#Entity,
    Omf#ReifiedRelationship,
    Omf#Entity,
    Set[java.lang.Throwable] \/ Omf#EntityExistentialRestrictionAxiom]

trait AddEntityDefinitionUniversalRestrictionAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function7[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLElement[Uml],
    UMLStereotype[Uml],
    Omf#Entity,
    Omf#ReifiedRelationship,
    Omf#Entity,
    Set[java.lang.Throwable] \/ Omf#EntityUniversalRestrictionAxiom]

trait AddEntityReifiedRelationshipSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function6[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLElement[Uml],
    Omf#ReifiedRelationship,
    UMLStereotype[Uml],
    Omf#ReifiedRelationship,
    Set[java.lang.Throwable] \/ Omf#ReifiedRelationshipSpecializationAxiom]

trait AddEntityConceptDesignationTerminologyGraphAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function5[
    MappingFunction[Uml, Omf, Provenance],
    TreeCompositeStructureType[Uml],
    Omf#MutableTerminologyBox,
    Omf#Concept,
    Omf#MutableTerminologyBox,
    Set[java.lang.Throwable] \/ Omf#ConceptDesignationTerminologyAxiom]

import TBoxMappingTuples._

/**
  * Result from applying a rule to a set of pairs.
  *
  * @param rule the rule to apply
  * @param finalResults pairs to be added as the result of this phase
  * @param internalResults pairs to be processed within this phase
  * @param externalResults pairs to be processed in the next phase (or as errors if this is the last phase rule)
  */
case class RuleResult[Uml <: UML, Omf <: OMF, Provenance]
( rule: MappingFunction[Uml, Omf, Provenance],
  finalResults: Vector[TboxUMLElementPair[Uml, Omf]],
  internalResults: Vector[TboxUMLElementPair[Uml, Omf]],
  externalResults: Vector[TboxUMLElementPair[Uml, Omf]] )


abstract class OTI2OMFMappingContext[Uml <: UML, Omf <: OMF, Provenance]
( val ignoreCrossReferencedElementFilter: Function1[UMLElement[Uml], Boolean],
  val iriPrefix: String,
  val tboxLookup: Namespace2TBoxLookupFunction[Uml, Omf],
  val ns2tboxCtor: Namespace2TBoxCtor[Uml, Omf, Provenance],

  val hasCanonicalNameDP: Omf#EntityScalarDataProperty,

  protected val element2aspectCtor: Element2AspectCTor[Uml, Omf, Provenance],
  protected val element2conceptCtor: Element2ConceptCTor[Uml, Omf, Provenance],
  protected val element2relationshipCtor: Element2RelationshipCTor[Uml, Omf, Provenance],

  protected val addDirectlyExtendedTerminologyGraphCtor: AddDirectlyExtendedTerminologyGraph[Uml, Omf, Provenance],
  protected val addDirectlyNestedTerminologyGraphCtor: AddDirectlyNestedTerminologyGraph[Uml, Omf, Provenance],
  protected val addEntityConceptDesignationTerminologyGraphAxiomCtor: AddEntityConceptDesignationTerminologyGraphAxiom[Uml, Omf, Provenance],

  protected val addEntityDefinitionAspectSubClassAxiomCtor: AddEntityDefinitionAspectSubClassAxiom[Uml, Omf, Provenance],
  protected val addEntityConceptSubClassAxiomCtor: AddEntityConceptSubClassAxiom[Uml, Omf, Provenance],
  protected val addEntityDefinitionExistentialRestrictionAxiomCtor: AddEntityDefinitionExistentialRestrictionAxiom[Uml, Omf, Provenance],
  protected val addEntityDefinitionUniversalRestrictionAxiomCtor: AddEntityDefinitionUniversalRestrictionAxiom[Uml, Omf, Provenance],
  protected val addEntityRelationshipSubClassAxiomCtor: AddEntityReifiedRelationshipSubClassAxiom[Uml, Omf, Provenance],

  val stereotype2Aspect: Map[UMLStereotype[Uml], Omf#Aspect],
  val stereotype2Concept: Map[UMLStereotype[Uml], Omf#Concept],
  val stereotype2Relationship: Map[UMLStereotype[Uml], Omf#ReifiedRelationship],

  val specializingProfiles: Set[UMLProfile[Uml]],
  val otherStereotypesApplied: ParMap[UMLStereotype[Uml], immutable.ParSet[UMLElement[Uml]]],
  val pkg2ont: Map[UMLPackage[Uml], Omf#ImmutableTerminologyBox],
  val pf2ont: Map[UMLProfile[Uml], Omf#ImmutableTerminologyBox],
  val rds: ResolvedDocumentSet[Uml],
  val ops: OMFOps[Omf],
  val store: Omf#Store,
  val treeOps: TreeOps[Uml],
  val idg: IDGenerator[Uml]) {

  implicit val umlOps = idg.umlOps
  import umlOps._
  import TBoxMappingTuples._

  protected val OML2OTIProvenances = new scala.collection.mutable.ListBuffer[OML2OTIProvenance]()

  def getOML2OTIProvenance
  : OML2OTIProvenanceTables
  = OML2OTIProvenanceTables
    .createEmptyOML2OTIProvenanceTables()
    .copy(oml2OTIProvenances = OML2OTIProvenances.to[Seq])

  val package2Document: Map[UMLPackage[Uml], Document[Uml]]
  = rds.ds.allDocuments.map { d => d.scope -> d }.toMap

  val package2SerializableDocument: Map[UMLPackage[Uml], Document[Uml] with SerializableDocument]
  = rds.ds.allSerializableDocuments.map { d => d.scope -> d }.toMap

  val package2BuiltInDocument: Map[UMLPackage[Uml], Document[Uml] with BuiltInDocument]
  = rds.ds.allBuiltInDocuments.map { d => d.scope -> d }.toMap

  def lookupDocumentPackageScopeAndTerminologyGraph
  (p: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#TerminologyBox)] = {
    val od
    : Option[Document[Uml]]
    = lookupDocumentByPackageScope(p)

    val result
    : Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#TerminologyBox)]
    = od
      .fold[Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#TerminologyBox)]]({
      Option.empty[(Document[Uml], Omf#TerminologyBox)].right
    }) { d =>
      d.scope match {
        case pf: UMLProfile[Uml] =>
          lookupImmutableModelTerminologyGraphByProfile(pf)
            .orElse(lookupMutableModelTerminologyGraphByProfile(pf))
            .fold[Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#TerminologyBox)]](
            Set(
              UMLError.illegalElementError[Uml, UMLProfile[Uml]](
                s"lookupDocumentPackageScopeAndTerminologyGraph: missing graph for element's profile: ${pf.qualifiedName.get}",
                Iterable(pf)
              )
            ).left
          ) { g =>
            (d, g).some.right
          }
        case pkg: UMLPackage[Uml] =>
          lookupImmutableModelTerminologyGraphByPackage(pkg)
            .orElse(lookupMutableModelTerminologyGraphByPackage(pkg))
            .fold[Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#TerminologyBox)]](
            Set(
              UMLError.illegalElementError[Uml, UMLPackage[Uml]](
                s"lookupDocumentPackageScopeAndTerminologyGraph: missing graph for element's package: ${pkg.qualifiedName.get}",
                Iterable(pkg)
              )
            ).left
          ) { g =>
            (d, g).some.right
          }
      }
    }

    result
  }

  def lookupDocumentByPackageScope(pkg: UMLPackage[Uml]): Option[Document[Uml]] = {
    val result
    : Option[Document[Uml]]
    = package2Document.get(pkg)

    result
  }

  def lookupImmutableModelTerminologyGraphByPackage
  (pkg: UMLPackage[Uml])
  : Option[Omf#ImmutableTerminologyBox] = {
    val result
    : Option[Omf#ImmutableTerminologyBox]
    = pkg2ont.get(pkg)

    result
  }

  def lookupMutableModelTerminologyGraphByPackage
  (pkg: UMLPackage[Uml])

  : Option[Omf#MutableTerminologyBox]

  def lookupModelTerminologyGraphByPackage
  (pkg: UMLPackage[Uml])
  : Option[Omf#TerminologyBox]
  = lookupImmutableModelTerminologyGraphByPackage(pkg)
    .orElse(lookupMutableModelTerminologyGraphByPackage(pkg))

  def lookupImmutableModelTerminologyGraphByProfile
  (pf: UMLProfile[Uml])
  : Option[Omf#ImmutableTerminologyBox] = {

    val result
    : Option[Omf#ImmutableTerminologyBox]
    = pf2ont.get(pf)

    result
  }

  def lookupMutableModelTerminologyGraphByProfile
  (pf: UMLProfile[Uml])
  : Option[Omf#MutableTerminologyBox]

  def lookupModelTerminologyGraphByProfile
  (pf: UMLProfile[Uml])
  : Option[Omf#TerminologyBox]
  = lookupImmutableModelTerminologyGraphByProfile(pf)
    .orElse(lookupMutableModelTerminologyGraphByProfile(pf))

  type UMLStereotype2EntityAspectMap = Map[UMLStereotype[Uml], Omf#Aspect]
  type UMLStereotype2EntityConceptMap = Map[UMLStereotype[Uml], Omf#Concept]
  type UMLStereotype2EntityRelationshipMap = Map[UMLStereotype[Uml], Omf#ReifiedRelationship]


  type RuleFunction =
  PartialFunction[
    ( MappingFunction[Uml, Omf, Provenance],
      TboxUMLElementPair[Uml, Omf],
      UMLStereotype2EntityAspectMap,
      UMLStereotype2EntityConceptMap,
      UMLStereotype2EntityRelationshipMap,
      Set[UMLStereotype[Uml]] ),
    Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]]

  type Element2AspectCTorRuleFunction = Function3[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLElement[Uml],
    Set[java.lang.Throwable] \/ Omf#Aspect]

  type Element2ConceptCTorRuleFunction = Function3[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLNamedElement[Uml],
    Set[java.lang.Throwable] \/ MappedEntityConcept]

  type Element2RelationshipCTorRuleFunction = Function7[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableTerminologyBox,
    UMLNamedElement[Uml],
    Omf#Entity,
    Omf#Entity,
    Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    Option[String],
    Set[java.lang.Throwable] \/ MappedEntityRelationship]

  type MappedEntityConcept = Omf#Concept
  type MappedEntityRelationship = Omf#ReifiedRelationship

  val abbrevName2Aspect = stereotype2Aspect map { case ( s, a ) =>
    s.name.get -> a
  }

  val abbrevName2Concept = stereotype2Concept map { case ( s, c ) =>
    s.name.get -> c
  }

  val abbrevName2Relationship = stereotype2Relationship map { case ( s, r ) =>
    s.name.get -> r
  }

  val abbrevName2Entity = abbrevName2Aspect ++ abbrevName2Concept ++ abbrevName2Relationship

  val allMappedStereotypes = stereotype2Aspect.keySet ++ stereotype2Concept.keySet ++ stereotype2Relationship.keySet

  def isStereotypeMapped( s: UMLStereotype[Uml] ): Boolean = {
    val result: Boolean = allMappedStereotypes.contains( s )

    result
  }

  def recordAppliedStereotype
  (e: UMLElement[Uml])
  (s: UMLStereotype[Uml])
  : Unit

  def isStereotypedElementMapped( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Boolean = {
    val isMapped
    : Set[java.lang.Throwable] \/ Boolean
    = e
      .getAppliedStereotypes
      .map { m2p =>
        m2p
          .keys
          .exists(isStereotypeMapped)
      }

    isMapped
  }

  def getAppliedStereotypesMappedToOMF
  ( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/
    ( UMLStereotype2EntityAspectMap,
      UMLStereotype2EntityConceptMap,
      UMLStereotype2EntityRelationshipMap,
      Set[UMLStereotype[Uml]] ) = {

    val result
    : Set[java.lang.Throwable] \/
      (UMLStereotype2EntityAspectMap,
        UMLStereotype2EntityConceptMap,
        UMLStereotype2EntityRelationshipMap,
        Set[UMLStereotype[Uml]])
    = e
      .getAppliedStereotypes
      .map { m2p =>

        val appliedStereotypes =
          m2p
            .keySet
            .filter { s =>
              !ignoreCrossReferencedElementFilter(s)
            }

        ( stereotype2Aspect.filterKeys(appliedStereotypes.contains),
          stereotype2Concept.filterKeys(appliedStereotypes.contains),
          stereotype2Relationship.filterKeys(appliedStereotypes.contains),
          appliedStereotypes -- allMappedStereotypes )
      }

    result
  }

  def getSortedAppliedStereotypesMappedToOMF
  ( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/
    ( List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]] ) = {

    val result
    : Set[java.lang.Throwable] \/
      ( List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]], List[UMLStereotype[Uml]] )
    = getAppliedStereotypesMappedToOMF(e)
      .map { case (as, cs, rs, us) =>
        (as.keys.toList.sortBy(_.qualifiedName.get),
          cs.keys.toList.sortBy(_.qualifiedName.get),
          rs.keys.toList.sortBy(_.qualifiedName.get),
          us.toList.sortBy(_.qualifiedName.get))
      }

    result
  }

  def doesStereotypedElementMap2Aspect( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Boolean = {

    val result
    : Set[java.lang.Throwable] \/ Boolean
    = getAppliedStereotypesMappedToOMF(e)
      .map { case (as, cs, _, _) =>
        as.nonEmpty && cs.isEmpty
      }

    result
  }

  def doesStereotypedElementMap2Concept( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Boolean = {

    val result
    : Set[java.lang.Throwable] \/ Boolean
    = getAppliedStereotypesMappedToOMF(e)
      .map(_._2.nonEmpty)

    result
  }

  def doesStereotypedElementMap2AspectOrConcept( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Boolean = {

    val result
    : Set[java.lang.Throwable] \/ Boolean
    = getAppliedStereotypesMappedToOMF(e)
      .map { case (as, cs, _, _) =>
        as.nonEmpty || cs.nonEmpty
      }

    result
  }

  def doesStereotypedElementMap2Relationship( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Boolean = {

    val result
    : Set[java.lang.Throwable] \/ Boolean
    = getAppliedStereotypesMappedToOMF(e)
      .map(_._3.nonEmpty)

    result
  }

  def partitionAppliedStereotypesByMapping
  ( e: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ ( Set[UMLStereotype[Uml]], Set[UMLStereotype[Uml]] ) = {

    val result
    : Set[java.lang.Throwable] \/ (Set[UMLStereotype[Uml]], Set[UMLStereotype[Uml]])
    = e
      .getAppliedStereotypes
      .map { m2p =>
        m2p
          .keySet
          .filter(s => !ignoreCrossReferencedElementFilter(s))
          .partition(isStereotypeMapped)
      }

    result
  }

  val (baseIdentifiedElementS, baseIdentifiedElementA) = {
    val pair = stereotype2Aspect.find { _._1.name.contains("base:IdentifiedElement") }
    require(pair.isDefined)
    pair.get
  }

  val hasCanonicalNameP: UMLProperty[Uml] = {
    val p =
      baseIdentifiedElementS
        .attribute
        .find { _.name.contains("hasCanonicalName") }
    require(p.isDefined)
    p.get
  }

  val baseIdentifiedElementOrSpecific: Set[UMLStereotype[Uml]] =
    closure[UMLStereotype[Uml], UMLStereotype[Uml]](
      baseIdentifiedElementS,
      _.general_classifier.selectByKindOf { case s: UMLStereotype[Uml] => s})

  def checkBaseIdentifiedElementOrSpecificAppliedStereotype
  (e: UMLElement[Uml])
  : Set[java.lang.Throwable] \/ Boolean
  = {
    val result
    : Set[java.lang.Throwable] \/ Boolean
    = e
      .getAppliedStereotypes
      .map { sp =>
        val isID = (sp.keySet & baseIdentifiedElementOrSpecific).nonEmpty
        isID
      }

    result
  }

  def getCanonicalNameIfAny
  (u: UMLNamedElement[Uml])
  : Set[java.lang.Throwable] \/ Option[String]
  = checkBaseIdentifiedElementOrSpecificAppliedStereotype(u)
    .flatMap { isIdentifiedElement =>
      if (!isIdentifiedElement)
        \/-(None)
      else
        u
        .getStereotypeTagPropertyStringValues(hasCanonicalNameP)
        .flatMap { tagValues =>
          require(tagValues.size <= 1)
          val cname: Option[String] = tagValues.headOption.orElse(u.name)
          \/-(cname)
        }
    }

  def mapCanonicalName
  (tbox: Omf#MutableTerminologyBox,
   eU: UMLElement[Uml],
   eC: Omf#Entity)
  : Set[java.lang.Throwable] \/ Option[Omf#EntityScalarDataPropertyParticularRestrictionAxiom]
  = eU match {
    case neU: UMLNamedElement[Uml] =>
      getCanonicalNameIfAny(neU).flatMap(
        _.fold[Set[java.lang.Throwable] \/ Option[Omf#EntityScalarDataPropertyParticularRestrictionAxiom]](
          \/-(None)
        ) { cname =>
          ops
            .addEntityScalarDataPropertyParticularRestrictionAxiom(tbox, eC, hasCanonicalNameDP, cname)(store)
            .map(Some(_))
        })
    case _ =>
      \/-(None)
  }

  val basePackageC = abbrevName2Concept( "base:Package" )

  val projectAuthorityC = abbrevName2Concept( "project:Authority" )
  val projectAuthorityS = {
    val pa = stereotype2Concept.keys.find { s => s.name.contains("project:Authority") }
    require(pa.isDefined, "Error: There must be an OMF:Concept stereotype named 'project:Authority'")
    pa.get
  }

  val projectAuthorityOrSpecific: Set[UMLStereotype[Uml]] =
    (closure[UMLStereotype[Uml], UMLStereotype[Uml]](
      projectAuthorityS,
      _.general_classifier.selectByKindOf { case s: UMLStereotype[Uml] => s }) + projectAuthorityS)
    .filterNot(_.isAbstract)

  def lookupProjectAuthorityOrSpecificAppliedStereotype
  (pkg: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Set[UMLStereotype[Uml]]
  = {
    val result
    : Set[java.lang.Throwable] \/ Set[UMLStereotype[Uml]]
    = pkg
      .getAppliedStereotypes
      .map { sp =>
        val pas = sp.keySet & projectAuthorityOrSpecific
        pas
      }

    result
  }

  def packageOrAuthority2TBox
  (rule: MappingFunction[Uml, Omf, Provenance],
   pair: TBoxOTIDocumentPackagePair[Uml, Omf],
   pkg2provenance: UMLPackage[Uml] => Provenance)
  : Set[java.lang.Throwable] \/ TBoxOTIDocumentPackageConversion[Uml, Omf]
  = {
    val pkgU = pair.e

    val result1
    : Set[java.lang.Throwable] \/ TBoxOTIDocumentPackageConversion[Uml, Omf]
    = for {
      pkgTbox <- ns2tboxCtor(rule, pkgU, TerminologyKind.isDefinition, pkg2provenance(pkgU))
      _ <- {
        val result2
        : Set[java.lang.Throwable] \/ Unit
        = if (pair.authorities.isEmpty)
          \/-(())
        else {
          require(pair.nestingPkgTbox.isDefined)
          val result3 =
            for {
              pkgAuthC <- mapElement2Concept(rule, pair.nestingPkgTbox.get, pkgU)
              _ <- {
                val a0: Set[java.lang.Throwable] \/ Unit = \/-(())
                val aN: Set[java.lang.Throwable] \/ Unit = (a0 /: pair.authorities) { (ai, authS) =>
                  require(stereotype2Concept.contains(authS))
                  val authC = stereotype2Concept(authS)
                  val inc
                  : Set[java.lang.Throwable] \/ Omf#ConceptSpecializationAxiom
                  = addEntityConceptSubClassAxiom(
                    rule, pkgU, s"packageOrAuthority2TBox(${authS.qualifiedName.get})",
                    pair.nestingPkgTbox.get, pkgU, pkgAuthC, authS, authC)
                  ai +++ inc.map(_ => ())
                }
                aN
              }
              _ <- addDirectlyNestedTerminologyGraph(rule, pkgU, pair.nestingPkgTbox.get, pkgAuthC, pkgTbox)
            } yield ()
          result3
        }
        result2
      }
      pkgConv = pair.toConversion(pkgTbox)
    } yield pkgConv

    result1
  }

  def nestedPackageOrAuthority2TBox
  (rule: MappingFunction[Uml, Omf, Provenance],
   pair: TBoxOTIDocumentPackageConversion[Uml, Omf],
   pkg2provenance: UMLPackage[Uml] => Provenance,
   nestedPkgAuthorities: Set[UMLStereotype[Uml]],
   nestedPkgU: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ TBoxOTIDocumentPackageConversion[Uml, Omf]
  = {
    val result
    : Set[java.lang.Throwable] \/ TBoxOTIDocumentPackageConversion[Uml, Omf]
    = for {
      nestedPkgTbox <- ns2tboxCtor(rule, nestedPkgU, TerminologyKind.isDefinition, pkg2provenance(nestedPkgU))
      nestedPkgAuthC <- mapElement2Concept(rule, pair.pkgDocumentTbox, nestedPkgU)
      _ <- {
        val a0: Set[java.lang.Throwable] \/ Unit = \/-(())
        val aN: Set[java.lang.Throwable] \/ Unit = (a0 /: nestedPkgAuthorities) { (ai, authS) =>
          require(stereotype2Concept.contains(authS))
          val authC = stereotype2Concept(authS)
          val inc
          : Set[java.lang.Throwable] \/ Unit
          = addEntityConceptSubClassAxiom(
            rule,
            nestedPkgU, s"nestedPackageOrAuthority2TBox(${authS.qualifiedName.get})",
            pair.pkgDocumentTbox, nestedPkgU, nestedPkgAuthC, authS, authC)
            .map(_ => ())
          ai +++ inc
        }
        aN
      }
      _ <- addDirectlyNestedTerminologyGraph(rule, nestedPkgU, pair.pkgDocumentTbox, nestedPkgAuthC, nestedPkgTbox)
      nestedPkgConv = pair.toNestedConversion(nestedPkgAuthorities, nestedPkgU, nestedPkgTbox)
    } yield nestedPkgConv

    result
  }

  val (baseAggregatesS, baseAggregatesR) = {
    val pair = stereotype2Relationship.find { _._1.name.contains("base:aggregates") }
    require(pair.isDefined)
    pair.get
  }

  val (baseContainsS, baseContainsR) = {
    val pair = stereotype2Relationship.find { _._1.name.contains("base:contains") }
    require(pair.isDefined)
    pair.get
  }

  protected val mappedElement2Aspect = scala.collection.mutable.HashMap[UMLElement[Uml], Omf#Aspect]()

  def lookupElementAspectMapping( e: UMLElement[Uml] ): Option[Omf#Aspect]
  = mappedElement2Aspect.get(e)

  def mapElement2Aspect
  ( rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Omf#Aspect
  = for {
    o <- element2aspectCtor.applyMapping(this, rule, tbox, u)
  } yield {
    OML2OTIProvenances += OTI2OMFMappingContext.OML2OTITermProvenance(ops, tbox, o, u, rule.name)
    mappedElement2Aspect += (u -> o)
    o
  }

  protected val mappedElement2Concept = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityConcept]()

  def lookupElementConceptMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#Concept]
  = mappedElement2Concept.get(e)

  def mapElement2Concept
  ( rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLNamedElement[Uml])
  : Set[java.lang.Throwable] \/ MappedEntityConcept
  = for {
    o <- element2conceptCtor.applyMapping(this, rule, tbox, u)
  } yield {
    OML2OTIProvenances += OTI2OMFMappingContext.OML2OTITermProvenance(ops, tbox, o, u, rule.name)
    mappedElement2Concept += (u -> o)
    o
  }

  protected val mappedElement2Relationship = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityRelationship]()

  def lookupElementRelationshipMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ReifiedRelationship]
  = mappedElement2Relationship.get(e)

  def mapElement2Relationship
  ( rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableTerminologyBox,
    u: UMLNamedElement[Uml],
    source: Omf#Entity,
    target: Omf#Entity,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    hasName: Option[String] )
  : Set[java.lang.Throwable] \/ MappedEntityRelationship
  = for {
    o <- element2relationshipCtor.applyMapping(this, rule, tbox, u, source, target, characteristics, hasName)
  } yield {
    OML2OTIProvenances += OTI2OMFMappingContext.OML2OTITermProvenance(ops, tbox, o, u, rule.name)
    mappedElement2Relationship += (u -> o)
    o
  }

  def lookupElementMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#Entity] = {
    val result
    : Option[Omf#Entity]
    = lookupElementAspectMapping(e) orElse
      lookupElementConceptMapping(e) orElse
      lookupElementRelationshipMapping(e)

    result
  }

  def getDependencySourceAndTargetMappings
  ( d: UMLDependency[Uml] )
  : Set[java.lang.Throwable] \&/
    ( ( UMLNamedElement[Uml], Option[Omf#Entity] ),
      ( UMLNamedElement[Uml], Option[Omf#Entity] ) )
  = for {
    sourceU <- if (1 == d.client.size)
      \&/.That(d.client.head)
    else
      \&/.This(Set(
        UMLError.illegalElementError[Uml, UMLDependency[Uml]](
          s"getDependencySourceAndTargetMappings: dependency should have exactly 1 client but it has: ${d.client.size}",
          Iterable(d)
        )))
    targetU <- if (1 == d.supplier.size)
      \&/.That(d.supplier.head)
    else
      \&/.This(Set(
        UMLError.illegalElementError[Uml, UMLDependency[Uml]](
          s"getDependencySourceAndTargetMappings: dependency should have exactly 1 supplier but it has: ${d.supplier.size}",
          Iterable(d)
        )))
    result = Tuple2(
      Tuple2( sourceU, lookupElementMapping(sourceU) ),
      Tuple2( targetU, lookupElementMapping(targetU) ) )
  } yield result

  def getDirectedBinaryAssociationSourceAndTargetMappings
  ( a: UMLAssociation[Uml] )
  : Option[
    ( ( UMLClassifier[Uml], Omf#Entity ),
      ( UMLClassifier[Uml], Omf#Entity ) )] = {

    val result
    : Option[
      ( ( UMLClassifier[Uml], Omf#Entity ),
        ( UMLClassifier[Uml], Omf#Entity ) )]
    = for {

      directedEnds <- a.getDirectedAssociationEnd
      (sourcePU, targetPU) = directedEnds

      sourceTU <- sourcePU._type.selectByKindOf { case cls: UMLClassifier[Uml] => cls }
      sourceE <- lookupElementMapping(sourceTU)

      targetTU <- targetPU._type.selectByKindOf { case cls: UMLClassifier[Uml] => cls }
      targetE <- lookupElementMapping(targetTU)

    } yield
      Tuple2(Tuple2(sourceTU, sourceE), Tuple2(targetTU, targetE))

    result
  }

  def addDirectlyExtendedTerminologyGraph
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   extendingG: Omf#MutableTerminologyBox,
   extendedG: Omf#TerminologyBox)
  : Set[java.lang.Throwable] \/ Unit
  = for {
    axiom <- addDirectlyExtendedTerminologyGraphCtor(rule, extendingG, extendedG)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITerminologyGraphAxiomProvenance(ops, extendingG, axiom, u, rule.name)(store)
    ()
  }

  def addDirectlyNestedTerminologyGraph
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   parentG: Omf#MutableTerminologyBox,
   parentC: Omf#Concept,
   nestedG: Omf#MutableTerminologyBox)
  : Set[java.lang.Throwable] \/ Unit
  = for {
    axiom <- addDirectlyNestedTerminologyGraphCtor(rule, parentG, parentC, nestedG)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITerminologyGraphAxiomProvenance(ops, nestedG, axiom, u, rule.name)(store)
    ()
  }

  def addEntityConceptDesignationTerminologyGraphAxiom
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   tree: TreeCompositeStructureType[Uml],
   tbox: Omf#MutableTerminologyBox,
   bstConcept: Omf#Concept,
   bstGraph: Omf#MutableTerminologyBox)
  (implicit omfStore: Omf#Store)
  : Set[java.lang.Throwable] \/ Unit
  = for {
    axiom <- addEntityConceptDesignationTerminologyGraphAxiomCtor(rule, tree, tbox, bstConcept, bstGraph)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITerminologyGraphAxiomProvenance(ops, bstGraph, axiom, u, rule.name)
    ()
  }

  def addEntityDefinitionAspectSubClassAxiom
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   explanation: String,
   tbox: Omf#MutableTerminologyBox,
   uSub: UMLElement[Uml],
   sub: Omf#Entity,
   uSup: UMLStereotype[Uml],
   sup: Omf#Aspect)
  : Set[java.lang.Throwable] \/ Omf#AspectSpecializationAxiom
  = for {
    axiom <- addEntityDefinitionAspectSubClassAxiomCtor(rule, tbox, uSub, sub, uSup, sup)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITermAxiomProvenance(ops, tbox, axiom, u, explanation)
    axiom
  }

  def addEntityConceptSubClassAxiom
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   explanation: String,
   tbox: Omf#MutableTerminologyBox,
   subU: UMLNamedElement[Uml],
   sub: Omf#Concept,
   supS: UMLStereotype[Uml],
   sup: Omf#Concept)
  : Set[java.lang.Throwable] \/ Omf#ConceptSpecializationAxiom
  = for {
    axiom <- addEntityConceptSubClassAxiomCtor(rule, tbox, subU, sub, supS, sup)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITermAxiomProvenance(ops, tbox, axiom, u, explanation)
    axiom
  }

  def addEntityDefinitionExistentialRestrictionAxiom
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   explanation: String,
   tbox: Omf#MutableTerminologyBox,
   contextU: UMLElement[Uml],
   contextS: UMLStereotype[Uml],
   domain: Omf#Entity,
   rel: Omf#ReifiedRelationship,
   range: Omf#Entity)
  : Set[java.lang.Throwable] \/ Omf#EntityExistentialRestrictionAxiom
  = for {
    axiom <- addEntityDefinitionExistentialRestrictionAxiomCtor(rule, tbox, contextU, contextS, domain, rel, range)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITermAxiomProvenance(ops, tbox, axiom, u, explanation)
    axiom
  }

  def addEntityDefinitionUniversalRestrictionAxiom
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   explanation: String,
   tbox: Omf#MutableTerminologyBox,
   contextU: UMLElement[Uml],
   contextS: UMLStereotype[Uml],
   domain: Omf#Entity,
   rel: Omf#ReifiedRelationship,
   range: Omf#Entity)
  : Set[java.lang.Throwable] \/ Omf#EntityUniversalRestrictionAxiom
  = for {
    axiom <- addEntityDefinitionUniversalRestrictionAxiomCtor(rule, tbox, contextU, contextS, domain, rel, range)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITermAxiomProvenance(ops, tbox, axiom, u, explanation)
    axiom
  }

  def addEntityRelationshipSubClassAxiom
  (rule: MappingFunction[Uml, Omf, Provenance],
   u: UMLElement[Uml],
   explanation: String,
   tbox: Omf#MutableTerminologyBox,
   subU: UMLElement[Uml],
   sub: Omf#ReifiedRelationship,
   supS: UMLStereotype[Uml],
   sup: Omf#ReifiedRelationship)
  : Set[java.lang.Throwable] \/ Omf#ReifiedRelationshipSpecializationAxiom
  = for {
    axiom <- addEntityRelationshipSubClassAxiomCtor(rule, tbox, subU, sub, supS, sup)
  } yield {
    OML2OTIProvenances +=
      OTI2OMFMappingContext.OML2OTITermAxiomProvenance(ops, tbox, axiom, u, explanation)
    axiom
  }
}

object OTI2OMFMappingContext {

  def OML2OTITermProvenance[Uml <: UML, Omf <: OMF, T <: Omf#Term]
  (ops: OMFOps[Omf],
   tbox: Omf#MutableTerminologyBox,
   o: T,
   u: UMLElement[Uml],
   explanation: String)
  : OML2OTIProvenance
  = OML2OTIProvenance(
    explanation,
    omlIRI = Some(ops.getTermIRI(o).toString),
    omlUUID = ops.getTermUUID(o).toString,
    otiID = OTIPrimitiveTypes.TOOL_SPECIFIC_ID.unwrap(u.toolSpecific_id),
    otiURL = OTIPrimitiveTypes.TOOL_SPECIFIC_URL.unwrap(u.toolSpecific_url),
    otiUUID = u.toolSpecific_uuid.map(OTIPrimitiveTypes.TOOL_SPECIFIC_UUID.unwrap)
  )

  def OML2OTITermAxiomProvenance[Uml <: UML, Omf <: OMF, A <: Omf#TermAxiom]
  (ops: OMFOps[Omf],
   tbox: Omf#MutableTerminologyBox,
   o: A,
   u: UMLElement[Uml],
   explanation: String)
  : OML2OTIProvenance
  = OML2OTIProvenance(
    explanation,
    omlIRI = None,
    omlUUID = ops.getAxiomUUID(o).toString,
    otiID = OTIPrimitiveTypes.TOOL_SPECIFIC_ID.unwrap(u.toolSpecific_id),
    otiURL = OTIPrimitiveTypes.TOOL_SPECIFIC_URL.unwrap(u.toolSpecific_url),
    otiUUID = u.toolSpecific_uuid.map(OTIPrimitiveTypes.TOOL_SPECIFIC_UUID.unwrap)
  )

  def OML2OTITerminologyGraphAxiomProvenance[Uml <: UML, Omf <: OMF]
  (ops: OMFOps[Omf],
   tbox: Omf#MutableTerminologyBox,
   o: Omf#TerminologyAxiom,
   u: UMLElement[Uml],
   explanation: String)
  (implicit omfStore: Omf#Store)
  : OML2OTIProvenance
  = OML2OTIProvenance(
    explanation,
    omlIRI = None,
    omlUUID = ops.getTerminologyAxiomUUID(o).toString,
    otiID = OTIPrimitiveTypes.TOOL_SPECIFIC_ID.unwrap(u.toolSpecific_id),
    otiURL = OTIPrimitiveTypes.TOOL_SPECIFIC_URL.unwrap(u.toolSpecific_url),
    otiUUID = u.toolSpecific_uuid.map(OTIPrimitiveTypes.TOOL_SPECIFIC_UUID.unwrap)
  )
}

case class OTI2OMFMapper[Uml <: UML, Omf <: OMF, Provenance]() {

  /**
   * Apply the first matching rule
   */
  def applyMatchingRule
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    current: TboxUMLElementPair[Uml, Omf],
    rules: List[MappingFunction[Uml, Omf, Provenance]] )
  : Set[java.lang.Throwable] \&/ Option[RuleResult[Uml, Omf, Provenance]]
  = {

    val result
    : Set[java.lang.Throwable] \&/ Option[RuleResult[Uml, Omf, Provenance]]
    = context
      .getAppliedStereotypesMappedToOMF(current.e)
      .fold[Set[java.lang.Throwable] \&/ Option[RuleResult[Uml, Omf, Provenance]]](
      (nels: Set[java.lang.Throwable]) =>
        \&/.This(nels),
      { case (as, cs, rs, us) =>
        val remaining =
          rules.dropWhile { r =>
            val isApplicable = r.mappingRule.isDefinedAt(Tuple6(r, current, as, cs, rs, us))
            !isApplicable
          }

        remaining match {
          case Nil =>
            \&/.That(Option.empty[RuleResult[Uml, Omf, Provenance]])
          case r :: _ =>
            r
              .mappingRule(Tuple6(r, current, as, cs, rs, us))
              .map(_.some)
        }
      })

    result
  }

  type RulesResult =
  ( Vector[TboxUMLElementPair[Uml, Omf]],
    Vector[TboxUMLElementPair[Uml, Omf]],
    Vector[TboxUMLElementPair[Uml, Omf]] )

  /**
   * Successively apply all matching rules to each content pair until
   * there are no follow-on namespaces to apply to or none of the rules match.
   */
  def applyAllRules
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    tbox: Option[Omf#MutableTerminologyBox],
    contentPairs: Vector[TboxUMLElementPair[Uml, Omf]],
    rules: List[MappingFunction[Uml, Omf, Provenance]] )
  ( implicit omfOps: OMFOps[Omf] )
  : Set[java.lang.Throwable] \&/ RulesResult = {

    @annotation.tailrec def step
    ( errors: Set[java.lang.Throwable],
      queue: Vector[TboxUMLElementPair[Uml, Omf]],
      results: Vector[TboxUMLElementPair[Uml, Omf]],
      deferred: Vector[TboxUMLElementPair[Uml, Omf]],
      outputs: Vector[TboxUMLElementPair[Uml, Omf]] )
    : Set[java.lang.Throwable] \&/ RulesResult
    = if (queue.isEmpty) {
      if (errors.isEmpty)
        \&/.That((results, deferred, outputs))
      else
        \&/.Both(errors, (results, deferred, outputs))
    } else {
      val (pair, pairs) = (queue.head, queue.tail)

      val ruleResult
      : Set[java.lang.Throwable] \&/ Option[RuleResult[Uml, Omf, Provenance]]
      = applyMatchingRule( context, pair, rules )

      val nextErrors = errors ++ ruleResult.a.getOrElse(Set())
      ruleResult.b.getOrElse(None) match {
        case None =>
          step( nextErrors, pairs, results, deferred :+ pair, outputs )
        case Some( RuleResult( rule, moreResults, morePairs, moreOutputs ) ) =>
          step( nextErrors, pairs ++ morePairs, results ++ moreResults, deferred, outputs ++ moreOutputs)
      }
    }

    val result
    : Set[java.lang.Throwable] \&/ RulesResult
    = step( Set[java.lang.Throwable](), contentPairs, Vector(), Vector(), Vector() )

    result
  }

}