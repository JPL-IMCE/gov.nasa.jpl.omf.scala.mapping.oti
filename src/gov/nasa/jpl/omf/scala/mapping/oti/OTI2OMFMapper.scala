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
package gov.nasa.jpl.omf.scala.mapping.oti

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.core.TerminologyKind._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.canonicalXMI.ResolvedDocumentSet
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.trees._
import org.omg.oti.uml.xmi._

import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.collection.parallel._
import scala.{annotation,Boolean,Enumeration,Function1,Function2,Function3,Function4,Function5,Function6,Function7,Function8}
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

trait Element2AspectCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Omf#ModelEntityAspect
}

trait Element2AspectCTorFunction[Uml <: UML, Omf <: OMF, Provenance]
  extends Element2AspectCTor[Uml, Omf, Provenance]
  with OTI2OMFMappingContext[Uml, Omf, Provenance]#Element2AspectCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Omf#ModelEntityAspect =
  for {
    aspect <- apply(rule, tbox, u)
    _ = context.mappedElement2Aspect += (u -> aspect)
  } yield aspect

}

trait Element2ConceptCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean)
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept
}

trait Element2ConceptCTorFunction[Uml <: UML, Omf <: OMF, Provenance]
  extends Element2ConceptCTor[Uml, Omf, Provenance]
  with OTI2OMFMappingContext[Uml, Omf, Provenance]#Element2ConceptCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean)
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept =
  for {
    conceptGraph <- apply(rule, tbox, u, isAbstract)
    _ = context.mappedElement2Concept += (u -> conceptGraph)
  } yield conceptGraph

}

trait Element2RelationshipCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: Option[String] )
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityRelationship
}

trait Element2RelationshipCTorFunction[Uml <: UML, Omf <: OMF, Provenance]
  extends Element2RelationshipCTor[Uml, Omf, Provenance]
  with OTI2OMFMappingContext[Uml, Omf, Provenance]#Element2RelationshipCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    name: Option[String] )
  : Set[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityRelationship =
  for {
    relationship <- apply(rule, tbox, u, source, target, characteristics, isAbstract, name)
    _ = context.mappedElement2Relationship += (u -> relationship)
  } yield relationship
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
    Set[java.lang.Throwable] \/ Omf#MutableModelTerminologyGraph]

trait AddDirectlyExtendedTerminologyGraph[Uml <: UML, Omf <: OMF, Provenance]
  extends Function3[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelTerminologyGraph,
    Set[java.lang.Throwable] \/ Unit]

trait AddDirectlyNestedTerminologyGraph[Uml <: UML, Omf <: OMF, Provenance]
  extends Function4[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityConcept,
    Omf#MutableModelTerminologyGraph,
    Set[java.lang.Throwable] \/ Unit]

trait AddEntityDefinitionAspectSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function6[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    Omf#ModelEntityDefinition,
    UMLStereotype[Uml],
    Omf#ModelEntityAspect,
    Set[java.lang.Throwable] \/ Omf#EntityDefinitionAspectSubClassAxiom]

trait AddEntityConceptSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function6[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLNamedElement[Uml],
    Omf#ModelEntityConcept,
    UMLStereotype[Uml],
    Omf#ModelEntityConcept,
    Set[java.lang.Throwable] \/ Omf#EntityConceptSubClassAxiom]

trait AddEntityDefinitionExistentialRestrictionAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function7[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    UMLStereotype[Uml],
    Omf#ModelEntityDefinition,
    Omf#ModelEntityReifiedRelationship,
    Omf#ModelEntityDefinition,
    Set[java.lang.Throwable] \/ Omf#EntityDefinitionExistentialRestrictionAxiom]

trait AddEntityDefinitionUniversalRestrictionAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function7[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    UMLStereotype[Uml],
    Omf#ModelEntityDefinition,
    Omf#ModelEntityReifiedRelationship,
    Omf#ModelEntityDefinition,
    Set[java.lang.Throwable] \/ Omf#EntityDefinitionUniversalRestrictionAxiom]

trait AddEntityReifiedRelationshipSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function6[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    Omf#ModelEntityReifiedRelationship,
    UMLStereotype[Uml],
    Omf#ModelEntityReifiedRelationship,
    Set[java.lang.Throwable] \/ Omf#EntityReifiedRelationshipSubClassAxiom]

trait AddEntityReifiedRelationshipContextualizationAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function8[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    UMLStereotype[Uml],
    Omf#ModelEntityDefinition,
    Omf#ModelEntityReifiedRelationship,
    String,
    Omf#ModelEntityDefinition,
    Set[java.lang.Throwable] \/ Omf#EntityReifiedRelationshipContextualizationAxiom]

trait AddEntityReifiedRelationshipRestrictionAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function8[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    UMLStereotype[Uml],
    Omf#ModelEntityDefinition,
    Omf#ModelEntityReifiedRelationship,
    Omf#ModelEntityDefinition,
    RestrictionKind,
    Set[java.lang.Throwable] \/ Omf#EntityReifiedRelationshipRestrictionAxiom]

trait AddEntityConceptDesignationTerminologyGraphAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function5[
    MappingFunction[Uml, Omf, Provenance],
    TreeCompositeStructureType[Uml],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityConcept,
    Omf#MutableModelTerminologyGraph,
    Set[java.lang.Throwable] \/ Omf#EntityConceptDesignationTerminologyGraphAxiom]

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

  val hasCanonicalNameDP: Omf#ModelDataRelationshipFromEntityToScalar,

  protected val element2aspectCtor: Element2AspectCTor[Uml, Omf, Provenance],
  protected val element2conceptCtor: Element2ConceptCTor[Uml, Omf, Provenance],
  protected val element2relationshipCtor: Element2RelationshipCTor[Uml, Omf, Provenance],

  val addDirectlyExtendedTerminologyGraph: AddDirectlyExtendedTerminologyGraph[Uml, Omf, Provenance],
  val addDirectlyNestedTerminologyGraph: AddDirectlyNestedTerminologyGraph[Uml, Omf, Provenance],
  val addEntityDefinitionAspectSubClassAxiom: AddEntityDefinitionAspectSubClassAxiom[Uml, Omf, Provenance],
  val addEntityConceptSubClassAxiom: AddEntityConceptSubClassAxiom[Uml, Omf, Provenance],
  val addEntityDefinitionExistentialRestrictionAxiom: AddEntityDefinitionExistentialRestrictionAxiom[Uml, Omf, Provenance],
  val addEntityDefinitionUniversalRestrictionAxiom: AddEntityDefinitionUniversalRestrictionAxiom[Uml, Omf, Provenance],
  val addEntityRelationshipSubClassAxiom: AddEntityReifiedRelationshipSubClassAxiom[Uml, Omf, Provenance],
  val addEntityRelationshipContextualizationAxiom: AddEntityReifiedRelationshipContextualizationAxiom[Uml, Omf, Provenance],
  val addReifiedRelationshipRestrictionAxiom: AddEntityReifiedRelationshipRestrictionAxiom[Uml, Omf, Provenance],
  val addEntityConceptDesignationTerminologyGraphAxiom: AddEntityConceptDesignationTerminologyGraphAxiom[Uml, Omf, Provenance],

  val stereotype2Aspect: Map[UMLStereotype[Uml], Omf#ModelEntityAspect],
  val stereotype2Concept: Map[UMLStereotype[Uml], Omf#ModelEntityConcept],
  val stereotype2Relationship: Map[UMLStereotype[Uml], Omf#ModelEntityReifiedRelationship],

  val specializingProfiles: Set[UMLProfile[Uml]],
  val otherStereotypesApplied: ParMap[UMLStereotype[Uml], immutable.ParSet[UMLElement[Uml]]],
  val pkg2ont: Map[UMLPackage[Uml], Omf#ImmutableModelTerminologyGraph],
  val pf2ont: Map[UMLProfile[Uml], Omf#ImmutableModelTerminologyGraph],
  val rds: ResolvedDocumentSet[Uml],
  val ops: OMFOps[Omf],
  val store: Omf#Store,
  val treeOps: TreeOps[Uml],
  val idg: IDGenerator[Uml]) {

  implicit val umlOps = idg.umlOps
  import umlOps._
  import ops._
  import TBoxMappingTuples._

  val package2Document: Map[UMLPackage[Uml], Document[Uml]]
  = rds.ds.allDocuments.map { d => d.scope -> d }.toMap

  val package2SerializableDocument: Map[UMLPackage[Uml], Document[Uml] with SerializableDocument]
  = rds.ds.allSerializableDocuments.map { d => d.scope -> d }.toMap

  val package2BuiltInDocument: Map[UMLPackage[Uml], Document[Uml] with BuiltInDocument]
  = rds.ds.allBuiltInDocuments.map { d => d.scope -> d }.toMap

  def lookupDocumentPackageScopeAndTerminologyGraph
  (p: UMLPackage[Uml])
  : Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)] = {
    val od
    : Option[Document[Uml]]
    = lookupDocumentByPackageScope(p)

    val result
    : Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)]
    = od
      .fold[Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)]]({
      Option.empty[(Document[Uml], Omf#ModelTerminologyGraph)].right
    }) { d =>
      d.scope match {
        case pf: UMLProfile[Uml] =>
          lookupImmutableModelTerminologyGraphByProfile(pf)
            .orElse(lookupMutableModelTerminologyGraphByProfile(pf))
            .fold[Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)]](
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
            .fold[Set[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)]](
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
  : Option[Omf#ImmutableModelTerminologyGraph] = {
    val result
    : Option[Omf#ImmutableModelTerminologyGraph]
    = pkg2ont.get(pkg)

    result
  }

  def lookupMutableModelTerminologyGraphByPackage
  (pkg: UMLPackage[Uml])

  : Option[Omf#MutableModelTerminologyGraph]

  def lookupModelTerminologyGraphByPackage
  (pkg: UMLPackage[Uml])
  : Option[Omf#ModelTerminologyGraph]
  = lookupImmutableModelTerminologyGraphByPackage(pkg)
    .orElse(lookupMutableModelTerminologyGraphByPackage(pkg))

  def lookupImmutableModelTerminologyGraphByProfile
  (pf: UMLProfile[Uml])
  : Option[Omf#ImmutableModelTerminologyGraph] = {

    val result
    : Option[Omf#ImmutableModelTerminologyGraph]
    = pf2ont.get(pf)

    result
  }

  def lookupMutableModelTerminologyGraphByProfile
  (pf: UMLProfile[Uml])
  : Option[Omf#MutableModelTerminologyGraph]

  def lookupModelTerminologyGraphByProfile
  (pf: UMLProfile[Uml])
  : Option[Omf#ModelTerminologyGraph]
  = lookupImmutableModelTerminologyGraphByProfile(pf)
    .orElse(lookupMutableModelTerminologyGraphByProfile(pf))

  type UMLStereotype2EntityAspectMap = Map[UMLStereotype[Uml], Omf#ModelEntityAspect]
  type UMLStereotype2EntityConceptMap = Map[UMLStereotype[Uml], Omf#ModelEntityConcept]
  type UMLStereotype2EntityRelationshipMap = Map[UMLStereotype[Uml], Omf#ModelEntityReifiedRelationship]


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
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    Set[java.lang.Throwable] \/ Omf#ModelEntityAspect]

  type Element2ConceptCTorRuleFunction = Function4[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLNamedElement[Uml],
    Boolean,
    Set[java.lang.Throwable] \/ MappedEntityConcept]

  type Element2RelationshipCTorRuleFunction = Function8[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLNamedElement[Uml],
    Omf#ModelEntityDefinition,
    Omf#ModelEntityDefinition,
    Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    Boolean,
    Option[String],
    Set[java.lang.Throwable] \/ MappedEntityRelationship]

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
  (tbox: Omf#MutableModelTerminologyGraph,
   eU: UMLElement[Uml],
   eC: Omf#ModelEntityDefinition)
  : Set[java.lang.Throwable] \/ Option[Omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral]
  = eU match {
    case neU: UMLNamedElement[Uml] =>
      getCanonicalNameIfAny(neU).flatMap(
        _.fold[Set[java.lang.Throwable] \/ Option[Omf#ModelScalarDataRelationshipRestrictionAxiomFromEntityToLiteral]](
          \/-(None)
        ) { cname =>
          ops
            .addScalarDataRelationshipRestrictionAxiomFromEntityToLiteral(tbox, eC, hasCanonicalNameDP, cname)(store)
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
    closure[UMLStereotype[Uml], UMLStereotype[Uml]](
      projectAuthorityS,
      _.general_classifier.selectByKindOf { case s: UMLStereotype[Uml] => s })

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
      pkgTbox <- ns2tboxCtor(rule, pkgU, TerminologyKind.isToplevelDefinition, pkg2provenance(pkgU))
      _ <- {
        val result2
        : Set[java.lang.Throwable] \/ Unit
        = if (pair.authorities.isEmpty)
          \/-(())
        else {
          require(pair.nestingPkgTbox.isDefined)
          val result3 =
            for {
              pkgAuthC <- mapElement2Concept(rule, pair.nestingPkgTbox.get, pkgU, isAbstract=false)
              _ <- {
                val a0: Set[java.lang.Throwable] \/ Unit = \/-(())
                val aN: Set[java.lang.Throwable] \/ Unit = (a0 /: pair.authorities) { (ai, authS) =>
                  require(stereotype2Concept.contains(authS))
                  val authC = stereotype2Concept(authS)
                  val inc
                  : Set[java.lang.Throwable] \/ Omf#EntityConceptSubClassAxiom
                  = addEntityConceptSubClassAxiom(rule, pair.nestingPkgTbox.get, pkgU, pkgAuthC, authS, authC)
                  ai +++ inc.map(_ => ())
                }
                aN
              }
              _ <- addDirectlyNestedTerminologyGraph(rule, pair.nestingPkgTbox.get, pkgAuthC, pkgTbox)
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
      nestedPkgAuthC <- mapElement2Concept(rule, pair.pkgDocumentTbox, nestedPkgU, isAbstract=false)
      _ <- {
        val a0: Set[java.lang.Throwable] \/ Unit = \/-(())
        val aN: Set[java.lang.Throwable] \/ Unit = (a0 /: nestedPkgAuthorities) { (ai, authS) =>
          require(stereotype2Concept.contains(authS))
          val authC = stereotype2Concept(authS)
          val inc
          : Set[java.lang.Throwable] \/ Unit
          = addEntityConceptSubClassAxiom(rule, pair.pkgDocumentTbox, nestedPkgU, nestedPkgAuthC, authS, authC).map(_ => ())
          ai +++ inc
        }
        aN
      }
      _ <- addDirectlyNestedTerminologyGraph(rule, pair.pkgDocumentTbox, nestedPkgAuthC, nestedPkgTbox)
      nestedPkgConv = pair.toNestedConversion(nestedPkgAuthorities, nestedPkgU, nestedPkgTbox)
    } yield nestedPkgConv

    result
  }

  val (baseContainsS, baseContainsR) = {
    val pair = stereotype2Relationship.find { _._1.name.contains("base:contains") }
    require(pair.isDefined)
    pair.get
  }

  val mappedElement2Aspect = scala.collection.mutable.HashMap[UMLElement[Uml], Omf#ModelEntityAspect]()
  def lookupElementAspectMapping( e: UMLElement[Uml] ): Option[Omf#ModelEntityAspect] = {

    val result
    : Option[Omf#ModelEntityAspect]
    = mappedElement2Aspect.get(e)

    result
  }

  def mapElement2Aspect
  ( rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLElement[Uml] )
  : Set[java.lang.Throwable] \/ Omf#ModelEntityAspect = {

    val result
    : Set[java.lang.Throwable] \/ Omf#ModelEntityAspect
    = element2aspectCtor.applyMapping(this, rule, tbox, u)

//    result.flatMap { entity =>
//      mapCanonicalName(tbox, u, entity).map { _ => entity }
//    }
    result
  }

  val mappedElement2Concept = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityConcept]()

  def lookupElementConceptMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ModelEntityConcept] = {
    val result: Option[Omf#ModelEntityConcept] = mappedElement2Concept.get(e)

    result
  }

  def mapElement2Concept
  ( rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean)
  : Set[java.lang.Throwable] \/ MappedEntityConcept = {
    val result
    : Set[java.lang.Throwable] \/ MappedEntityConcept
    = element2conceptCtor.applyMapping(this, rule, tbox, u, isAbstract)

//    result.flatMap { entity =>
//      mapCanonicalName(tbox, u, entity).map { _ => entity }
//    }
    result
  }

  val mappedElement2Relationship = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityRelationship]()

  def lookupElementRelationshipMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ModelEntityReifiedRelationship] = {

    val result
    : Option[Omf#ModelEntityReifiedRelationship]
    = mappedElement2Relationship.get(e)

    result
  }

  def mapElement2Relationship
  ( rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    source: Omf#ModelEntityDefinition,
    target: Omf#ModelEntityDefinition,
    characteristics: Iterable[RelationshipCharacteristics.RelationshipCharacteristics],
    isAbstract: Boolean,
    hasName: Option[String] )
  : Set[java.lang.Throwable] \/ MappedEntityRelationship = {
    val result
    : Set[java.lang.Throwable] \/ MappedEntityRelationship
    = element2relationshipCtor.applyMapping(
      this, rule, tbox, u, source, target, characteristics, isAbstract, hasName)

//    result.flatMap { entity =>
//      mapCanonicalName(tbox, u, entity).map { _ => entity }
//    }
    result
  }

  def lookupElementMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ModelEntityDefinition] = {
    val result
    : Option[Omf#ModelEntityDefinition]
    = lookupElementAspectMapping(e) orElse
      lookupElementConceptMapping(e) orElse
      lookupElementRelationshipMapping(e)

    result
  }

  def getDependencySourceAndTargetMappings
  ( d: UMLDependency[Uml] )
  : ( ( UMLNamedElement[Uml], Option[Omf#ModelEntityDefinition] ),
      ( UMLNamedElement[Uml], Option[Omf#ModelEntityDefinition] ) ) = {
    val sourceU = {
      require( d.client.size == 1 )
      d.client.head
    }
    val targetU = {
      require( d.supplier.size == 1 )
      d.supplier.head
    }

    Tuple2(
      Tuple2( sourceU, lookupElementMapping(sourceU) ),
      Tuple2( targetU, lookupElementMapping(targetU) ) )
  }

  def getDirectedBinaryAssociationSourceAndTargetMappings
  ( a: UMLAssociation[Uml] )
  : Option[
    ( ( UMLClassifier[Uml], Omf#ModelEntityDefinition ),
      ( UMLClassifier[Uml], Omf#ModelEntityDefinition ) )] = {

    val result
    : Option[
      ( ( UMLClassifier[Uml], Omf#ModelEntityDefinition ),
        ( UMLClassifier[Uml], Omf#ModelEntityDefinition ) )]
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
    tbox: Option[Omf#MutableModelTerminologyGraph],
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