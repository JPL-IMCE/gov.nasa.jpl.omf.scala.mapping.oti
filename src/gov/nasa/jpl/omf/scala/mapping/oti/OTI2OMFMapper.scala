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
import org.omg.oti.uml.canonicalXMI.ResolvedDocumentSet
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

trait Element2AspectCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect
}

trait Element2AspectCTorFunction[Uml <: UML, Omf <: OMF, Provenance]
  extends Element2AspectCTor[Uml, Omf, Provenance]
  with OTI2OMFMappingContext[Uml, Omf, Provenance]#Element2AspectCTorRuleFunction {

  override def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect =
  for {
    aspect <- apply(rule, tbox, u)
    sizePre = context.mappedElement2Aspect.size
    _ = context.mappedElement2Aspect += (u -> aspect)
    sizeDelta = context.mappedElement2Aspect.size - sizePre
    _ = java.lang.System.out.println(s"## mappedElement2Aspect $sizePre => +$sizeDelta")
  } yield aspect

}

trait Element2ConceptCTor[Uml <: UML, Omf <: OMF, Provenance] {
  def applyMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLNamedElement[Uml],
    isAbstract: Boolean)
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept
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
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept =
  for {
    conceptGraph <- apply(rule, tbox, u, isAbstract)
    sizePre = context.mappedElement2Concept.size
    _ = context.mappedElement2Concept += (u -> conceptGraph)
    sizeDelta = context.mappedElement2Concept.size - sizePre
    _ = java.lang.System.out.println(s"## mappedElement2Concept $sizePre => +$sizeDelta")
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
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityRelationship
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
  : NonEmptyList[java.lang.Throwable] \/ OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityRelationship =
  for {
    relationship <- apply(rule, tbox, u, source, target, characteristics, isAbstract, name)
    sizePre = context.mappedElement2Relationship.size
    _ = context.mappedElement2Relationship += (u -> relationship)
    sizeDelta = context.mappedElement2Relationship.size - sizePre
    _ = java.lang.System.out.println(s"## mappedElement2Relationship $sizePre => +$sizeDelta")
  } yield relationship
}

sealed abstract class TboxUMLElementPair[Uml <: UML, Omf <: OMF]
( val tbox: Option[Omf#ModelTerminologyGraph],
  val e: UMLElement[Uml] )
( implicit omfOps: OMFOps[Omf] )

sealed abstract class TboxUMLElement2EntityDefinition[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  val omfEntity: Omf#ModelEntityDefinition,
  override val e: UMLElement[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e )

case class TboxUMLElement2AspectDefinition[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val omfEntity: Omf#ModelEntityAspect,
  override val e: UMLElement[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElement2EntityDefinition[Uml, Omf]( tbox, omfEntity, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"${e.xmiElementLabel} / OMF EntityAspect Tuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"${e.xmiElementLabel} / OMF EntityAspect Tuple [tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}] entity: $omfEntity"
    }
}

case class TboxUMLPackage2ConceptDefinition[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val omfEntity: Omf#ModelEntityConcept,
  override val e: UMLPackage[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElement2EntityDefinition[Uml, Omf]( tbox, omfEntity, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"${e.xmiElementLabel} / OMF PackageConcept Tuple[tbox=<none>, ${e.xmiType.head}: ${e.qualifiedName.get}]"
    ){ g =>
      s"${e.xmiElementLabel} / OMF PackageConcept Tuple [tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.qualifiedName.get}] entity: $omfEntity"
    }
}

case class TboxUMLElement2ConceptDefinition[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val omfEntity: Omf#ModelEntityConcept,
  override val e: UMLElement[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElement2EntityDefinition[Uml, Omf]( tbox, omfEntity, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"${e.xmiElementLabel} / OMF EntityConcept Tuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"${e.xmiElementLabel} / OMF EntityConcept Tuple [tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}] entity: $omfEntity"
    }
}

case class TboxUMLElement2ReifiedRelationshipDefinition[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val omfEntity: Omf#ModelEntityReifiedRelationship,
  override val e: UMLElement[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElement2EntityDefinition[Uml, Omf]( tbox, omfEntity, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"${e.xmiElementLabel} / OMF EntityReifiedRelationship Tuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"${e.xmiElementLabel} / OMF EntityReifiedRelationship Tuple [tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}] entity: $omfEntity"
    }
}

// @todo Is this case possible at all?
case class TboxUMLPackage2MutableTBoxTuple[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val e: UMLPackage[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"TboxUMLPackage2MutableTBoxTuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"TboxUMLPackage2MutableTBoxTuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    }
}

case class TboxUMLPackage2MutableTBoxConversion[Uml <: UML, Omf <: OMF, Provenance]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val e: UMLPackage[Uml],
  val pkgOTIDocument: Document[Uml],
  val pkgDocumentTbox: Omf#ModelTerminologyGraph,
  val pkgConcept: OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept,
  val superConcepts: Set[Omf#ModelEntityConcept])
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"TboxUMLPackage2MutableTBoxConversion[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"TboxUMLPackage2MutableTBoxConversion[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    }
}

case class TboxUMLPackage2ImmutableTBoxTuple[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#ImmutableModelTerminologyGraph],
  override val e: UMLPackage[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"TboxUMLPackage2ImmutableTBoxTuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"TboxUMLPackage2ImmutableTBoxTuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    }
}

case class TboxUMLProfile2MutableTBoxTuple[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#MutableModelTerminologyGraph],
  override val e: UMLProfile[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"TboxUMLProfile2MutableTBoxTuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"TboxUMLProfile2MutableTBoxTuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    }
}

case class TboxUMLProfile2ImmutableTBoxTuple[Uml <: UML, Omf <: OMF]
( override val tbox: Option[Omf#ImmutableModelTerminologyGraph],
  override val e: UMLProfile[Uml] )
( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

  override def toString: String =
    tbox
      .fold[String](
      s"TboxUMLProfile2ImmutableTBoxTuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    ){ g =>
      s"TboxUMLProfile2ImmutableTBoxTuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
    }
}

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
    NonEmptyList[java.lang.Throwable] \/ Omf#MutableModelTerminologyGraph]

trait AddDirectlyExtendedTerminologyGraph[Uml <: UML, Omf <: OMF, Provenance]
  extends Function3[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelTerminologyGraph,
    NonEmptyList[java.lang.Throwable] \/ Unit]

trait AddDirectlyNestedTerminologyGraph[Uml <: UML, Omf <: OMF, Provenance]
  extends Function3[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    Omf#MutableModelTerminologyGraph,
    NonEmptyList[java.lang.Throwable] \/ Unit]

trait AddEntityDefinitionAspectSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function4[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityDefinition,
    Omf#ModelEntityAspect,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityDefinitionAspectSubClassAxiom]

trait AddEntityConceptSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function4[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityConcept,
    Omf#ModelEntityConcept,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityConceptSubClassAxiom]

trait AddEntityReifiedRelationshipSubClassAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function4[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityReifiedRelationship,
    Omf#ModelEntityReifiedRelationship,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityReifiedRelationshipSubClassAxiom]

trait AddEntityConceptDesignationTerminologyGraphAxiom[Uml <: UML, Omf <: OMF, Provenance]
  extends Function5[
    MappingFunction[Uml, Omf, Provenance],
    TreeCompositeStructureType[Uml],
    Omf#MutableModelTerminologyGraph,
    Omf#ModelEntityConcept,
    Omf#MutableModelTerminologyGraph,
    NonEmptyList[java.lang.Throwable] \/ Omf#EntityConceptDesignationTerminologyGraphAxiom]

abstract class OTI2OMFMappingContext[Uml <: UML, Omf <: OMF, Provenance]
( val ignoreCrossReferencedElementFilter: Function1[UMLElement[Uml], Boolean],
  val iriPrefix: String,
  val tboxLookup: Namespace2TBoxLookupFunction[Uml, Omf],
  val ns2tboxCtor: Namespace2TBoxCtor[Uml, Omf, Provenance],

  protected val element2aspectCtor: Element2AspectCTor[Uml, Omf, Provenance],
  protected val element2conceptCtor: Element2ConceptCTor[Uml, Omf, Provenance],
  protected val element2relationshipCtor: Element2RelationshipCTor[Uml, Omf, Provenance],

  val addDirectlyExtendedTerminologyGraph: AddDirectlyExtendedTerminologyGraph[Uml, Omf, Provenance],
  val addDirectlyNestedTerminologyGraph: AddDirectlyNestedTerminologyGraph[Uml, Omf, Provenance],
  val addEntityDefinitionAspectSubClassAxiom: AddEntityDefinitionAspectSubClassAxiom[Uml, Omf, Provenance],
  val addEntityConceptSubClassAxiom: AddEntityConceptSubClassAxiom[Uml, Omf, Provenance],
  val addEntityRelationshipSubClassAxiom: AddEntityReifiedRelationshipSubClassAxiom[Uml, Omf, Provenance],
  val addEntityConceptDesignationTerminologyGraphAxiom: AddEntityConceptDesignationTerminologyGraphAxiom[Uml, Omf, Provenance],

  val stereotype2Aspect: Map[UMLStereotype[Uml], Omf#ModelEntityAspect],
  val stereotype2Concept: Map[UMLStereotype[Uml], Omf#ModelEntityConcept],
  val stereotype2Relationship: Map[UMLStereotype[Uml], Omf#ModelEntityReifiedRelationship],
  val otherStereotypesApplied: Set[UMLStereotype[Uml]],
  val pkg2ont: Map[UMLPackage[Uml], Omf#ImmutableModelTerminologyGraph],
  val pf2ont: Map[UMLProfile[Uml], Omf#ImmutableModelTerminologyGraph],
  val rds: ResolvedDocumentSet[Uml],
  val ops: OMFOps[Omf],
  val treeOps: TreeOps[Uml],
  val idg: IDGenerator[Uml]) {

  implicit val umlOps = idg.umlOps
  import umlOps._
  import ops._

  val package2SerializableDocument: Map[UMLPackage[Uml], SerializableDocument[Uml]] =
    rds.ds.serializableDocuments.map { d => d.scope -> d }.toMap

  val package2BuiltInDocument: Map[UMLPackage[Uml], BuiltInDocument[Uml]] =
    rds.ds.builtInDocuments.map { d => d.scope -> d }.toMap

  def lookupDocumentPackageScopeAndTerminologyGraph
  (e: UMLElement[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)] =
  rds.element2mappedDocument(e)
  .fold[NonEmptyList[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)]](
    Option.empty[(Document[Uml], Omf#ModelTerminologyGraph)].right
  ){ d =>
    d.scope match {
      case pf: UMLProfile[Uml] =>
        lookupImmutableModelTerminologyGraphByProfile(pf)
        .orElse(lookupMutableModelTerminologyGraphByProfile(pf))
        .fold[NonEmptyList[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)]](
         NonEmptyList(
          UMLError.illegalElementError[Uml, UMLElement[Uml]](
            s"lookupDocumentPackageScopeAndTerminologyGraph: missing graph for element's profile: ${pf.qualifiedName.get}",
            Iterable(e, pf)
          )
         ).left
        ){ g =>
          (d, g).some.right
        }
      case pkg: UMLPackage[Uml] =>
        lookupImmutableModelTerminologyGraphByPackage(pkg)
          .orElse(lookupMutableModelTerminologyGraphByPackage(pkg))
          .fold[NonEmptyList[java.lang.Throwable] \/ Option[(Document[Uml], Omf#ModelTerminologyGraph)]](
          NonEmptyList(
            UMLError.illegalElementError[Uml, UMLElement[Uml]](
              s"lookupDocumentPackageScopeAndTerminologyGraph: missing graph for element's package: ${pkg.qualifiedName.get}",
              Iterable(e, pkg)
            )
          ).left
        ){ g =>
          (d, g).some.right
        }
    }
  }

  def lookupDocumentByPackageScope(pkg: UMLPackage[Uml]): Option[Document[Uml]] =
    package2SerializableDocument.get(pkg).orElse(package2BuiltInDocument.get(pkg))

  def lookupImmutableModelTerminologyGraphByPackage(pkg: UMLPackage[Uml]): Option[Omf#ImmutableModelTerminologyGraph] =
    pkg2ont.get(pkg)

  def lookupMutableModelTerminologyGraphByPackage(pkg: UMLPackage[Uml]): Option[Omf#MutableModelTerminologyGraph]

  def lookupImmutableModelTerminologyGraphByProfile(pf: UMLProfile[Uml]): Option[Omf#ImmutableModelTerminologyGraph] =
    pf2ont.get(pf)

  def lookupMutableModelTerminologyGraphByProfile(pf: UMLProfile[Uml]): Option[Omf#MutableModelTerminologyGraph]

  type UMLStereotype2EntityAspectMap = Map[UMLStereotype[Uml], Omf#ModelEntityAspect]
  type UMLStereotype2EntityConceptMap = Map[UMLStereotype[Uml], Omf#ModelEntityConcept]
  type UMLStereotype2EntityRelationshipMap = Map[UMLStereotype[Uml], Omf#ModelEntityReifiedRelationship]
  type TboxUMLElementPairs = List[TboxUMLElementPair[Uml, Omf]]
  type TboxUMLElementTriplePairs = ( TboxUMLElementPairs, TboxUMLElementPairs, TboxUMLElementPairs )
  type RuleFunction =
  PartialFunction[
    ( MappingFunction[Uml, Omf, Provenance],
      TboxUMLElementPair[Uml, Omf],
      UMLStereotype2EntityAspectMap,
      UMLStereotype2EntityConceptMap,
      UMLStereotype2EntityRelationshipMap,
      Set[UMLStereotype[Uml]] ),
    NonEmptyList[java.lang.Throwable] \/ TboxUMLElementTriplePairs]

  type Element2AspectCTorRuleFunction = Function3[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLElement[Uml],
    NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect]

  type Element2ConceptCTorRuleFunction = Function4[
    MappingFunction[Uml, Omf, Provenance],
    Omf#MutableModelTerminologyGraph,
    UMLNamedElement[Uml],
    Boolean,
    NonEmptyList[java.lang.Throwable] \/ MappedEntityConcept]

  type Element2RelationshipCTorRuleFunction = Function8[
    MappingFunction[Uml, Omf, Provenance],
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
  ( rule: MappingFunction[Uml, Omf, Provenance],
    tbox: Omf#MutableModelTerminologyGraph,
    u: UMLElement[Uml] )
  : NonEmptyList[java.lang.Throwable] \/ Omf#ModelEntityAspect =
    element2aspectCtor.applyMapping( this, rule, tbox, u )

  val mappedElement2Concept = scala.collection.mutable.HashMap[UMLElement[Uml], MappedEntityConcept]()

  def lookupElementConceptMapping
  ( e: UMLElement[Uml] )
  : Option[Omf#ModelEntityConcept] =
    mappedElement2Concept.get( e )

  def mapElement2Concept
  ( rule: MappingFunction[Uml, Omf, Provenance],
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
  ( rule: MappingFunction[Uml, Omf, Provenance],
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

case class OTI2OMFMapper[Uml <: UML, Omf <: OMF, Provenance]() {

  /**
   * A rule result is a 3-tuple:
   * - the rule itself that produced the results
   * - a list of Tbox / UML Element pairs to be added as the result of this phase
   * - a list of Tbox / UML Element pairs to be processed within this phase 
   * - a list of Tbox / UML Element pairs to be processed in the next phase (or as errors)
   */
  type RuleResult =
  ( MappingFunction[Uml, Omf, Provenance],
    OTI2OMFMappingContext[Uml, Omf, Provenance]#TboxUMLElementPairs,
    OTI2OMFMappingContext[Uml, Omf, Provenance]#TboxUMLElementPairs,
    OTI2OMFMappingContext[Uml, Omf, Provenance]#TboxUMLElementPairs )
  
  /**
   * Apply the first matching rule
   */
  def applyMatchingRule
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    current: TboxUMLElementPair[Uml, Omf],
    rules: List[MappingFunction[Uml, Omf, Provenance]] )
  : NonEmptyList[java.lang.Throwable] \/ Option[RuleResult] =
    context.getAppliedStereotypesMappedToOMF( current.e )
    .flatMap { case ( as, cs, rs, us ) =>
      val remaining =
        rules.dropWhile { r =>
          val isApplicable = r.mappingRule.isDefinedAt(Tuple6(r, current, as, cs, rs, us))
          !isApplicable
        }

      remaining match {
        case Nil =>
          Option.empty[RuleResult]
            .right
        case r :: _ =>
          r
          .mappingRule(Tuple6(r, current, as, cs, rs, us))
          .map { case (pairs1, pairs2, pairs3) =>
            val result: RuleResult = (r, pairs1, pairs2, pairs3)
            result.some
          }
      }
    }

  type RulesResult =
  ( OTI2OMFMappingContext[Uml, Omf, Provenance]#TboxUMLElementPairs,
    OTI2OMFMappingContext[Uml, Omf, Provenance]#TboxUMLElementPairs,
    OTI2OMFMappingContext[Uml, Omf, Provenance]#TboxUMLElementPairs )
  
  /**
   * Successively apply all matching rules to each content pair until
   * there are no follow-on namespaces to apply to or none of the rules match.
   */
  def applyAllRules
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    tbox: Option[Omf#MutableModelTerminologyGraph],
    contentPairs: List[TboxUMLElementPair[Uml, Omf]],
    rules: List[MappingFunction[Uml, Omf, Provenance]] )
  ( implicit omfOps: OMFOps[Omf] )
  : NonEmptyList[java.lang.Throwable] \&/ RulesResult = {

    @annotation.tailrec def step
    ( errors: Option[NonEmptyList[java.lang.Throwable]],
      queue: List[TboxUMLElementPair[Uml, Omf]],
      results: List[TboxUMLElementPair[Uml, Omf]],
      deferred: List[TboxUMLElementPair[Uml, Omf]],
      outputs: List[TboxUMLElementPair[Uml, Omf]] )
    : NonEmptyList[java.lang.Throwable] \&/ RulesResult =
      queue match {
        case Nil =>
          \&/.That( ( results, deferred, outputs ) )
        case pair :: pairs =>
          applyMatchingRule( context, pair, rules ) match {
            case -\/( f ) =>
              step( errors.map(f append _), pairs, results, deferred, outputs )
            case \/-( None ) =>
              step( errors, pairs, results, pair :: deferred, outputs )
            case \/-( Some( ( rule, moreResults, morePairs, moreOutputs ) ) ) =>
              step( errors, pairs ::: morePairs, moreResults ::: results, deferred, moreOutputs ::: outputs )
          }
      }

    step( Option.empty[NonEmptyList[java.lang.Throwable]], contentPairs, Nil, Nil, Nil )
  }

}