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

import gov.nasa.jpl.omf.scala.core._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.trees._
import org.omg.oti.uml.xmi._

import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.{Option,Some,StringContext}

object TBoxMappingTuples {

  sealed abstract class TboxUMLElementPair[Uml <: UML, Omf <: OMF]
  ( val tbox: Option[Omf#ModelTerminologyGraph],
    val e: UMLElement[Uml] )
  ( implicit omfOps: OMFOps[Omf] )

  sealed abstract class TBoxOTIDocumentProfileTuple[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLProfile[Uml],
    val authorities: Set[UMLStereotype[Uml]],
    val pfOTIDocument: Document[Uml] )
  ( implicit omfOps: OMFOps[Omf] )
  extends TboxUMLElementPair[Uml, Omf]( tbox, e )

  case class TBoxOTIDocumentProfilePair[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLProfile[Uml],
    override val authorities: Set[UMLStereotype[Uml]],
    override val pfOTIDocument: Document[Uml])
  ( implicit omfOps: OMFOps[Omf] )
    extends TBoxOTIDocumentProfileTuple[Uml, Omf]( tbox, e, authorities, pfOTIDocument) {

    def toConversion
    (pfDocumentTbox: Omf#MutableModelTerminologyGraph)
    : TBoxOTIDocumentProfileConversion[Uml, Omf]
    = TBoxOTIDocumentProfileConversion(tbox, e, authorities, pfOTIDocument, pfDocumentTbox)

    def toConverted
    (pfDocumentTbox: Omf#ImmutableModelTerminologyGraph)
    : TBoxOTIDocumentProfileConverted[Uml, Omf]
    = TBoxOTIDocumentProfileConverted(tbox, e, authorities, pfOTIDocument, pfDocumentTbox)

  }

  case class TBoxOTIDocumentProfileConversion[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLProfile[Uml],
    override val authorities: Set[UMLStereotype[Uml]],
    override val pfOTIDocument: Document[Uml],
    pfDocumentTbox: Omf#MutableModelTerminologyGraph)
  ( implicit omfOps: OMFOps[Omf] )
    extends TBoxOTIDocumentProfileTuple[Uml, Omf]( tbox, e, authorities, pfOTIDocument ) {

    override def toString: String =
      s"TBoxOTIDocumentProfileConversion[tbox=${omfOps.getTerminologyGraphIRI( pfDocumentTbox )}, "+
      s"${e.xmiType.head}: ${e.toolSpecific_id}]"
  }

  case class TBoxOTIDocumentProfileConverted[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLProfile[Uml],
    override val authorities: Set[UMLStereotype[Uml]],
    override val pfOTIDocument: Document[Uml],
    pfDocumentTbox: Omf#ImmutableModelTerminologyGraph)
  ( implicit omfOps: OMFOps[Omf] )
    extends TBoxOTIDocumentProfileTuple[Uml, Omf]( tbox, e, authorities, pfOTIDocument ) {

    override def toString: String =
      s"TBoxOTIDocumentProfileConverted[tbox=${omfOps.getTerminologyGraphIRI( pfDocumentTbox )}, "+
      s"${e.xmiType.head}: ${e.toolSpecific_id}]"

  }

  sealed abstract class TBoxOTIDocumentPackageTuple[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLPackage[Uml],
    val authorities: Set[UMLStereotype[Uml]],
    val pkgOTIDocument: Document[Uml],
    val nestingPkgTbox: Option[Omf#MutableModelTerminologyGraph] )
  ( implicit omfOps: OMFOps[Omf] )
    extends TboxUMLElementPair[Uml, Omf]( tbox, e )

  case class TBoxOTIDocumentPackagePair[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLPackage[Uml],
    override val authorities: Set[UMLStereotype[Uml]],
    override val pkgOTIDocument: Document[Uml],
    override val nestingPkgTbox: Option[Omf#MutableModelTerminologyGraph])
  ( implicit omfOps: OMFOps[Omf] )
    extends TBoxOTIDocumentPackageTuple[Uml, Omf]( tbox, e, authorities, pkgOTIDocument, nestingPkgTbox ) {

    def toConversion
    (pkgDocumentTbox: Omf#MutableModelTerminologyGraph)
    : TBoxOTIDocumentPackageConversion[Uml, Omf]
    = TBoxOTIDocumentPackageConversion(tbox, e, authorities, pkgOTIDocument, nestingPkgTbox, pkgDocumentTbox)

    def toConverted
    (pkgDocumentTbox: Omf#ImmutableModelTerminologyGraph)
    : TBoxOTIDocumentPackageConverted[Uml, Omf]
    = TBoxOTIDocumentPackageConverted(tbox, e, authorities, pkgOTIDocument, nestingPkgTbox, pkgDocumentTbox)

  }

  case class TBoxOTIDocumentPackageConversion[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLPackage[Uml],
    override val authorities: Set[UMLStereotype[Uml]],
    override val pkgOTIDocument: Document[Uml],
    override val nestingPkgTbox: Option[Omf#MutableModelTerminologyGraph],
    pkgDocumentTbox: Omf#MutableModelTerminologyGraph)
  ( implicit omfOps: OMFOps[Omf] )
    extends TBoxOTIDocumentPackageTuple( tbox, e, authorities, pkgOTIDocument, nestingPkgTbox ) {

    def toContentsConversion
    ()
    : TBoxPackageContentsConversion[Uml, Omf]
    = TBoxPackageContentsConversion(tbox, e, pkgDocumentTbox)

    def toNestedConversion
    (authorities: Set[UMLStereotype[Uml]],
     nestedPkgU: UMLPackage[Uml],
     nestedPkgDocumentTbox: Omf#MutableModelTerminologyGraph)
    : TBoxOTIDocumentPackageConversion[Uml, Omf]
    = copy(e=nestedPkgU, nestingPkgTbox=Some(this.pkgDocumentTbox), pkgDocumentTbox=nestedPkgDocumentTbox)

    override def toString: String =
      s"TBoxOTIDocumentPackageConversion[tbox=${omfOps.getTerminologyGraphIRI( pkgDocumentTbox )}, "+
      s"${e.xmiType.head}: ${e.toolSpecific_id}]"

  }

  case class TBoxOTIDocumentPackageConverted[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLPackage[Uml],
    override val authorities: Set[UMLStereotype[Uml]],
    override val pkgOTIDocument: Document[Uml],
    override val nestingPkgTbox: Option[Omf#MutableModelTerminologyGraph],
    pkgDocumentTbox: Omf#ImmutableModelTerminologyGraph)
  ( implicit omfOps: OMFOps[Omf] )
    extends TBoxOTIDocumentPackageTuple( tbox, e, authorities, pkgOTIDocument, nestingPkgTbox ) {

    override def toString: String =
      s"TBoxOTIDocumentPackageConverted[tbox=${omfOps.getTerminologyGraphIRI( pkgDocumentTbox )}, "+
      s"${e.xmiType.head}: ${e.toolSpecific_id}]"

  }

  case class TBoxPackageContentsConversion[Uml <: UML, Omf <: OMF]
  ( override val tbox: Some[Omf#MutableModelTerminologyGraph],
    override val e: UMLPackage[Uml],
    pkgDocumentTbox: Omf#MutableModelTerminologyGraph)
  ( implicit omfOps: OMFOps[Omf] )
    extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

    override def toString: String =
      s"TBoxPackageContentsConversion[tbox=${omfOps.getTerminologyGraphIRI( pkgDocumentTbox )}, "+
        s"${e.xmiType.head}: ${e.toolSpecific_id}]"

  }

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
        s"${e.xmiElementLabel} / OMF EntityAspect Tuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: "+
          s"${e.toolSpecific_id}] entity: $omfEntity"
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
        s"${e.xmiElementLabel} / OMF PackageConcept Tuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.qualifiedName.get}] entity: $omfEntity"
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
        s"${e.xmiElementLabel} / OMF EntityConcept Tuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}] entity: $omfEntity"
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
        s"${e.xmiElementLabel} / OMF EntityReifiedRelationship Tuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}] entity: $omfEntity"
      }
  }

  case class TboxUMLElement2ReifiedRelationshipContextualization[Uml <: UML, Omf <: OMF]
  ( override val tbox: Option[Omf#MutableModelTerminologyGraph],
    override val omfEntity: Omf#ModelEntityReifiedRelationship,
    override val e: UMLElement[Uml],
    umlDomain: UMLElement[Uml],
    omfDomain: Omf#ModelEntityDefinition,
    umlRange: UMLElement[Uml],
    omfRange: Omf#ModelEntityDefinition,
    contextName: String)
  ( implicit omfOps: OMFOps[Omf] )
    extends TboxUMLElement2EntityDefinition[Uml, Omf]( tbox, omfEntity, e ) {

    override def toString: String =
      tbox
        .fold[String](
        s"${e.xmiElementLabel} / OMF TboxUMLElement2ReifiedRelationshipContextualization Tuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
      ){ g =>
        s"${e.xmiElementLabel} / OMF TboxUMLElement2ReifiedRelationshipContextualization Tuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}] entity: $omfEntity"
      }
  }

  case class TboxUMLElement2ReifiedRelationshipRestriction[Uml <: UML, Omf <: OMF]
  ( override val tbox: Option[Omf#MutableModelTerminologyGraph],
    override val omfEntity: Omf#ModelEntityReifiedRelationship,
    override val e: UMLElement[Uml],
    umlDomain: UMLElement[Uml],
    omfDomain: Omf#ModelEntityDefinition,
    umlRange: UMLElement[Uml],
    omfRange: Omf#ModelEntityDefinition,
    kind: RestrictionKind)
  ( implicit omfOps: OMFOps[Omf] )
    extends TboxUMLElement2EntityDefinition[Uml, Omf]( tbox, omfEntity, e ) {

    override def toString: String =
      tbox
        .fold[String](
        s"${e.xmiElementLabel} / OMF TboxUMLElement2ReifiedRelationshipRestriction Tuple[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
      ){ g =>
        s"${e.xmiElementLabel} / OMF TboxUMLElement2ReifiedRelationshipRestriction Tuple[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}] entity: $omfEntity"
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
    pkgOTIDocument: Document[Uml],
    pkgDocumentTbox: Omf#ModelTerminologyGraph,
    pkgConcept: OTI2OMFMappingContext[Uml, Omf, Provenance]#MappedEntityConcept,
    superConcepts: Set[Omf#ModelEntityConcept])
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

  case class TboxUMLProfile2MutableTBoxConversion[Uml <: UML, Omf <: OMF, Provenance]
  ( override val tbox: Option[Omf#MutableModelTerminologyGraph],
    override val e: UMLProfile[Uml],
    pfOTIDocument: Document[Uml],
    pfDocumentTbox: Omf#ModelTerminologyGraph)
  ( implicit omfOps: OMFOps[Omf] )
    extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

    override def toString: String =
      tbox
        .fold[String](
        s"TboxUMLProfile2MutableTBoxConversion[tbox=<none>, ${e.xmiType.head}: ${e.toolSpecific_id}]"
      ){ g =>
        s"TboxUMLProfile2MutableTBoxConversion[tbox=${omfOps.getTerminologyGraphIRI( g )}, ${e.xmiType.head}: ${e.toolSpecific_id}]"
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

  case class TboxUMLNestedClassifier[Uml <: UML, Omf <: OMF]
  ( override val tbox: Option[Omf#MutableModelTerminologyGraph],
    override val e: UMLClassifier[Uml],
    nestingClass: UMLClass[Uml])
  ( implicit omfOps: OMFOps[Omf] )
    extends TboxUMLElementPair[Uml, Omf]( tbox, e ) {

    override def toString: String =
      tbox
        .fold[String](
        s"NestedClassifier[tbox=<none>, nestingParent=${nestingClass.qualifiedName.get}, nestedChild=${e.qualifiedName.get}]"
      ){ g =>
        s"NestedClassifier[tbox=${omfOps.getTerminologyGraphIRI( g )}, , nestingParent=${nestingClass.qualifiedName.get}, nestedChild=${e.qualifiedName.get}]"
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

}