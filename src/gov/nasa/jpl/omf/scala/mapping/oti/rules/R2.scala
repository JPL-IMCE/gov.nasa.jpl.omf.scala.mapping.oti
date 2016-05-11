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
import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples.{TboxUMLElementTuple, TboxUMLNestedClassifier}
import gov.nasa.jpl.omf.scala.mapping.oti._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.trees._

import scala.{Some, StringContext, Tuple3, Unit}
import scala.collection.immutable._
import scala.language.postfixOps
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

        val result
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = {
          if (unmappedS.nonEmpty) {
            val foreign = unmappedS.filter(!context.otherStereotypesApplied.contains(_))
            if (foreign.nonEmpty) {
              System.out.println(s"*** R2 WARN: ignoring ${foreign.size} unrecognized stereotypes applied to Aspect: ${nsU.qualifiedName.get}")
              foreign.foreach { s =>
                System.out.println(s"***  ignoring ${s.qualifiedName.get}")
              }
            }
          }

          val result
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
                finalResults=aspectPair,
                internalResults=Vector(),
                externalResults=Vector() // @todo enable when there are data property mapping rules aspectPair
              )
            )
          } yield aspectResult


          result.toThese
        }

        result
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
//        System.out
//          .println(s"#OTI/OMF R2 class2concept: ${c.qualifiedName.get} / a:${as.size}, c:${cs.size}")

        val r0
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = \&/.That(RuleResult(rule, Vector(), Vector(), Vector()))

        val rA
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = (r0 /: as) {
          case (acc, (ai, aiOmf)) =>
//            System.out.println(
//              s"""|#OTI/OMF R2 EntityDefinitionAspectSubClassAxiom:
//                  | sup=${ai.name.get}, sub=${c.qualifiedName.get}""".stripMargin)

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
//            System.out.println(
//              s"""|#OTI/OMF R2 EntityConceptSubClassAxiom:
//                  | sup=${ci.name.get}, sub=${c.qualifiedName.get}""".stripMargin)

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

//        System.out.println(s"#OTI/OMF R2 class2concept: error? ${rC.isLeft} ok? ${rC.isRight}")

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
                        System.out.println(s"#OTI/OMF R2 class2concept => RBST OK")
                        val conceptPair = TboxUMLElementTreeType(Some(tbox), cConcept, bst)
                        \&/.That(result.copy(
                          // this is a result
                          finalResults = result.finalResults :+ conceptPair,
                          // it needs to be further expanded in the next phase
                          externalResults = result.externalResults :+ conceptPair))
                      } else {
                        System.out.println(s"#OTI/OMF R2 class2concept => RBST ${problems.size} problems")
                        \&/.Both(Set(
                          treeOpsException(
                            context.treeOps,
                            problems
                              .map(_.toString)
                              .mkString(s"TreeCompositeStructure has ${problems.size} problems\n", "\n", "\n"))),
                          result)
                      }
                    case tree =>
                      System.out.println(s"#OTI/OMF R2 class2concept => other $tree")
                      \&/.Both(Set(
                        treeOpsException(
                          context.treeOps,
                          s"Not a TreeCompositeStructureType: $tree")),
                        result)
                  }
              else {
                System.out.println(s"#OTI/OMF R2 class2concept => not RBST")
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
        //          System.out
        //            .println(s"#OTI/OMF R2 other2concept: ${cls.qualifiedName.get} / a:${as.size}, c:${cs.size}")

        val r0
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = \&/.That(RuleResult(rule, Vector(), Vector(), Vector()))

        val rA
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = (r0 /: as) {
          case (acc, (ai, aiOmf)) =>
            //              System.out.println(
            //                s"""|#OTI/OMF R2 EntityDefinitionAspectSubClassAxiom:
            //                    | sup=${ai.name.get}, sub=${cls.qualifiedName.get}""".stripMargin)
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
            //              System.out.println(
            //                s"""|#OTI/OMF R2 EntityConceptSubClassAxiom:
            //                    | sup=${ci.name.get}, sub=${cls.qualifiedName.get}""".stripMargin)

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
        = {
          if (unmappedS.nonEmpty) {
            val foreign = unmappedS.filter(!context.otherStereotypesApplied.contains(_))
            if (foreign.nonEmpty) {
              System.out.println(s"*** R2 WARN: ignoring ${foreign.size} unrecognized stereotypes applied to Concept: ${neU.qualifiedName.get}")
              foreign.foreach { s =>
                System.out.println(s"***  ignoring ${s.qualifiedName.get}")
              }
            }
          }

          neU match {
            case c: UMLClass[Uml] =>
              class2concept(rule, tbox, c, as, cs)
            case cls: UMLClassifier[Uml] =>
              other2concept(rule, tbox, cls, as, cs)
            case _ =>
              System.out.println(s"#OTI/OMF R2 EntityConcept => unknown: ${neU.xmiElementLabel} ${neU.toolSpecific_id}")
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
        }

        result
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
          collectNestedElements(e, expandNestedElements _)
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