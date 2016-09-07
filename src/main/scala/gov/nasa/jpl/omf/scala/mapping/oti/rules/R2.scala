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
import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples.TboxUMLElementTuple
import gov.nasa.jpl.omf.scala.mapping.oti._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.trees._

import scala.{Some, StringContext}
import scala.collection.immutable._
import scalaz._
import Scalaz._

/**
 * Mapping for a kind of UML Namespace (but not a kind of UML Package) to
 * an OMF aspect or concept entity (but not relationship)
 *
 * The mapping of a UML Namespace depends on the IMCE-generated profile stereotypes applied (or specializations of).
 * There are 4 kinds of stereotypes:
 * 1) as: Stereotypes that directly or indirectly map into a kind of OMF EntityAspect
 * 2) cs: Stereotypes that directly or indirectly map into a kind of OMF EntityConcept
 * 3) rs: Stereotypes that directly or indirectly map into a kind of OMF EntityRelationship
 * 4) Stereotypes that have no direct or indirect mapping into any kind of
 * OMF EntityDefinition (Aspect, Concept or Relationship)
 *
 * `namespace2AspectMapping` applies only for (1) -- i.e., some as, no cs, no rs
 * `namedElement2ConceptMapping` applies for (2) -- i.e., some cs, no rs
 */
case class R2[Uml <: UML, Omf <: OMF, Provenance]()(implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf]) {

  /**
   * Map an OTI UMLNamespace as an OMF aspect according to the mapping of the stereotypes applied.
   */
  def namespace2AspectMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (rule, TboxUMLElementTuple(Some(tbox), nsU: UMLNamespace[Uml]), as, cs, rs, unmappedS)
        if as.nonEmpty && cs.isEmpty && rs.isEmpty =>

        val unmappedErrors
        : Set[java.lang.Throwable]
        = for {
          s <- unmappedS
          if !context.otherStereotypesApplied.contains(s)
          error =
          UMLError.illegalElementError[Uml, UMLNamespace[Uml]](
            s"R2 ignoring unrecognized stereotype application: <<${s.qualifiedName.get}>> to aspect: ${nsU.qualifiedName.getOrElse(nsU.toolSpecific_id)}",
            Iterable(nsU))
        } yield error

        val result
        : Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]
        = for {
          clsOmfAspect <- context.mapElement2Aspect(rule, tbox, nsU)

          _ = as.foreach {
            case (aS, aOmf) =>
              context.addEntityDefinitionAspectSubClassAxiom(rule, tbox, nsU, clsOmfAspect, aS, aOmf)
          }

          aspectPair = Vector(TboxUMLElement2AspectDefinition(Some(tbox), clsOmfAspect, nsU))
          aspectResult = scanForNestedElements(
            context, tbox, nsU,
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults = aspectPair,
              internalResults = Vector(),
              externalResults = Vector() // @todo enable when there are data property mapping rules aspectPair
            )
          )
        } yield aspectResult

        if (unmappedErrors.isEmpty)
          result.toThese
        else
          result match {
            case -\/(errors) =>
              \&/.This(errors ++ unmappedErrors)
            case \/-(r) =>
              \&/.Both(unmappedErrors, r)
          }
    }

    MappingFunction[Uml, Omf, Provenance]("namespace2AspectMapping", mapping)

  }

  /**
   * Map an OTI UMLNamedElement as an OMF concept according to the mapping of the stereotypes applied.
   */
  def namedElement2ConceptMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

    def class2concept
    (rule: MappingFunction[Uml, Omf, Provenance],
     tbox: Omf#MutableModelTerminologyGraph,
     c: UMLClass[Uml],
     as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
     cs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap)
    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
    = context
      .mapElement2Concept(rule, tbox, c, c.isAbstract)
      .toThese
      .flatMap { cConcept =>

        val r0
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = \&/.That(RuleResult(rule, Vector(), Vector(), Vector()))

        val rA
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = (r0 /: as) {
          case (acc, (ai, aiOmf)) =>

            acc.flatMap { ri =>
              context
                .addEntityDefinitionAspectSubClassAxiom(rule, tbox, c, cConcept, ai, aiOmf)
                .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                (nels: Set[java.lang.Throwable]) =>
                  \&/.Both(
                    Set(UMLError.illegalElementException[Uml, UMLClass[Uml]](
                      s"R2 (addEntityDefinitionAspectSubClassAxiom)",
                      Iterable(c, ai),
                      nels)),
                    ri),
                (_: Omf#EntityDefinitionAspectSubClassAxiom) =>
                  \&/.That(ri))
            }
        }

        val rC
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = (rA /: cs) {
          case (acc, (ciS, ciOmf)) =>

            acc.flatMap { rj =>
              context
                .addEntityConceptSubClassAxiom(rule, tbox, c, cConcept, ciS, ciOmf)
                .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                (nels: Set[java.lang.Throwable]) =>
                  \&/.Both(
                    Set(UMLError.illegalElementException[Uml, UMLClass[Uml]](
                      s"R2 (addEntityConceptSubClassAxiom)",
                      Iterable(c, ciS),
                      nels)),
                    rj),
                (_: Omf#EntityConceptSubClassAxiom) =>
                  \&/.That(rj))
            }
        }

        rC.flatMap { result =>

          context.treeOps.isRootBlockSpecificType(c)
            .toThese
            .flatMap { isRBST =>

              if (isRBST)
                analyze(c)(context.treeOps, context.idg, context.idg.otiCharacteristicsProvider)
                  .toThese
                  .flatMap {
                    case bst: TreeCompositeStructureType[Uml] =>
                      val problems = TreeType.getIllFormedTreeBranchPairs(bst)
                      if (problems.isEmpty) {
                        val conceptPair = TboxUMLElementTreeType(Some(tbox), cConcept, bst)
                        \&/.That(result.copy(
                          // this is a result
                          finalResults = result.finalResults :+ conceptPair,
                          // it needs to be further expanded in the next phase
                          externalResults = result.externalResults :+ conceptPair))
                      } else {
                        \&/.Both(Set(
                          treeOpsException(
                            context.treeOps,
                            problems
                              .map(_.toString)
                              .mkString(s"TreeCompositeStructure has ${problems.size} problems\n", "\n", "\n"))),
                          result)
                      }
                    case tree =>
                      \&/.Both(Set(
                        treeOpsException(
                          context.treeOps,
                          s"Not a TreeCompositeStructureType: $tree")),
                        result)
                  }
              else {
                val conceptPair = TboxUMLElement2ConceptDefinition(Some(tbox), cConcept, c)
                \&/.That(
                  scanForNestedElements(
                    context, tbox, c,
                  result.copy(
                  // this is a result
                  finalResults = result.finalResults :+ conceptPair,
                  // @todo enable when there are data property mapping rules conceptPair
                  externalResults = result.externalResults))
                )
              }
            }
        }
      }

    def other2concept
    (rule: MappingFunction[Uml, Omf, Provenance],
     tbox: Omf#MutableModelTerminologyGraph,
     cls: UMLClassifier[Uml],
     as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
     cs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap)
    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
    = context
      .mapElement2Concept(rule, tbox, cls, cls.isAbstract)
      .toThese
      .flatMap { cConcept =>

        val r0
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = \&/.That(RuleResult(rule, Vector(), Vector(), Vector()))

        val rA
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = (r0 /: as) {
          case (acc, (ai, aiOmf)) =>
            acc.flatMap { ri =>
              context
                .addEntityDefinitionAspectSubClassAxiom(rule, tbox, cls, cConcept, ai, aiOmf)
                .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                (nels: Set[java.lang.Throwable]) =>
                  \&/.Both(
                    Set(UMLError.illegalElementException[Uml, UMLClassifier[Uml]](
                      s"R2 (addEntityDefinitionAspectSubClassAxiom)",
                      Iterable(cls, ai),
                      nels)),
                    ri),
                (_: Omf#EntityDefinitionAspectSubClassAxiom) =>
                  \&/.That(ri))
            }
        }

        val rC
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = (rA /: cs) {
          case (acc, (ciS, ciOmf)) =>
            acc.flatMap { rj =>
              context
                .addEntityConceptSubClassAxiom(rule, tbox, cls, cConcept, ciS, ciOmf)
                .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                (nels: Set[java.lang.Throwable]) =>
                  \&/.Both(
                    Set(UMLError.illegalElementException[Uml, UMLClassifier[Uml]](
                      s"R2 (addEntityConceptSubClassAxiom)",
                      Iterable(cls, ciS),
                      nels)),
                    rj),
                (_: Omf#EntityConceptSubClassAxiom) =>
                  \&/.That(rj))
            }
        }

        rC.flatMap { result =>
          val conceptPair = TboxUMLElement2ConceptDefinition(Some(tbox), cConcept, cls)
          \&/.That(
            scanForNestedElements(
              context, tbox, cls,
              result.copy(
                // this is a result
                finalResults = result.finalResults :+ conceptPair,
                // @todo enable when there are data property mapping rules conceptPair
                externalResults = result.externalResults
              ))
          )
        }
      }

    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (rule, TboxUMLElementTuple(Some(tbox), neU: UMLNamedElement[Uml]), as, cs, rs, unmappedS)
        if cs.nonEmpty && rs.isEmpty =>

        val result
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = neU match {
            case c: UMLClass[Uml] =>
              class2concept(rule, tbox, c, as, cs)
            case cls: UMLClassifier[Uml] =>
              other2concept(rule, tbox, cls, as, cs)
            case _ =>
              \&/.This(Set(
                treeOpsException(
                  context.treeOps,
                  s"R2 is not applicable to: $neU",
                  UMLError
                    .illegalElementError[Uml, UMLElement[Uml]]("#OTI/OMF R2 Element2Concept not a Class:" +
                    neU.qualifiedName.get + ": " +
                    neU.xmiType.head +
                    s" / a:${as.size}, c:${cs.size}, r:${rs.size}", Iterable(neU)))
              ))
          }

        val unmappedErrors
        : Set[java.lang.Throwable]
        = for {
          s <- unmappedS
          if !context.otherStereotypesApplied.contains(s)
          error =
          UMLError.illegalElementError[Uml, UMLNamedElement[Uml]](
            s"R2 ignoring unrecognized stereotype application: <<${s.qualifiedName.get}>> to: ${neU.qualifiedName.getOrElse(neU.toolSpecific_id)}",
            Iterable(neU))
        } yield error

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

    MappingFunction[Uml, Omf, Provenance]("namedElement2ConceptMapping", mapping)

  }

  def scanForNestedElements
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   tbox: Omf#MutableModelTerminologyGraph,
   e: UMLElement[Uml],
   result: RuleResult[Uml, Omf, Provenance])
  : RuleResult[Uml, Omf, Provenance]
  = result.copy(
      internalResults =
        result.internalResults ++
          collectNestedElements(e, expandNestedElements)
            .map(TboxUMLElementTuple(Some(tbox), _)))

  def expandNestedElements
  (e: UMLElement[Uml])
  : (Vector[UMLElement[Uml]], Vector[UMLElement[Uml]])
  = {
    val expand = e.ownedElement.filter {
      case _: UMLPackage[Uml] => true
      case _ => false
    }
      .toVector
    val result = e.ownedElement.filter {
      case _: UMLFeature[Uml] => false
      case _: UMLPackage[Uml] => false
      case _ => true
    }
      .toVector
    (expand, result)
  }

  def collectNestedElements
  (e: UMLElement[Uml],
   expand: UMLElement[Uml] => (Vector[UMLElement[Uml]], Vector[UMLElement[Uml]]))
  : Vector[UMLElement[Uml]]
  = {

    @scala.annotation.tailrec
    def collectNestedElements
    (queue: Vector[UMLElement[Uml]],
     acc: Vector[UMLElement[Uml]])
    (implicit expand: UMLElement[Uml] => (Vector[UMLElement[Uml]], Vector[UMLElement[Uml]]))
    : Vector[UMLElement[Uml]]
    = if (queue.isEmpty)
      acc
    else {
      val (e, rest) = (queue.head, queue.drop(1))
      val (more, results) = expand(e)
      collectNestedElements(more ++ rest, acc ++ results)
    }

    collectNestedElements(Vector(e), Vector())(expand)
  }
}