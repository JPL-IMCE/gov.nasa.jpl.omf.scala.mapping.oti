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
import gov.nasa.jpl.omf.scala.mapping.oti._

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.trees._

import scala.{Some,StringContext,Tuple3,Unit}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._, Scalaz._

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
            require(foreign.isEmpty)
          }

          val result
          = for {
            clsOmfAspect <- context.mapElement2Aspect(rule, tbox, nsU)

            _ = as.foreach {
              case (aS, aOmf) =>
                context.addEntityDefinitionAspectSubClassAxiom(rule, tbox, clsOmfAspect, aOmf)
            }

            aspectPair = Vector(TboxUMLElement2AspectDefinition(Some(tbox), clsOmfAspect, nsU))
          } yield
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults=aspectPair,
              internalResults=Vector(),
              externalResults=Vector() // @todo enable when there are data property mapping rules aspectPair
            )

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
    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance] =
      context.mapElement2Concept(rule, tbox, c, c.isAbstract)
      .flatMap { cConcept =>
//        System.out
//          .println(s"#OTI/OMF R2 class2concept: ${c.qualifiedName.get} / a:${as.size}, c:${cs.size}")

        val r0: Set[java.lang.Throwable] \/ Unit = \/-(())
        val rA: Set[java.lang.Throwable] \/ Unit = (r0 /: as) {
          case (ri, (ai, aiOmf)) =>
//            System.out.println(
//              s"""|#OTI/OMF R2 EntityDefinitionAspectSubClassAxiom:
//                  | sup=${ai.name.get}, sub=${c.qualifiedName.get}""".stripMargin)
            ri +++
              context
                .addEntityDefinitionAspectSubClassAxiom(rule, tbox, cConcept, aiOmf)
                .map(_ => ())
        }

        val rC: Set[java.lang.Throwable] \/ Unit = (rA /: cs) {
          case (ri, (ci, ciOmf)) =>
//            System.out.println(
//              s"""|#OTI/OMF R2 EntityConceptSubClassAxiom:
//                  | sup=${ci.name.get}, sub=${c.qualifiedName.get}""".stripMargin)
            ri +++
              context
                .addEntityConceptSubClassAxiom(rule, tbox, cConcept, ciOmf)
                .map(_ => ())
        }

//        System.out.println(s"#OTI/OMF R2 class2concept: error? ${rC.isLeft} ok? ${rC.isRight}")
        val result
        : Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]  =
          context.treeOps.isRootBlockSpecificType(c)
            .leftMap[Set[java.lang.Throwable]](_.toList.toSet)
            .flatMap { isRBST =>

              if (isRBST)
                analyze(c)(context.treeOps, context.idg, context.idg.otiCharacteristicsProvider)
                  .leftMap[Set[java.lang.Throwable]](_.toList.toSet)
                  .flatMap {
                    case bst: TreeCompositeStructureType[Uml] =>
                      val problems = TreeType.getIllFormedTreeBranchPairs(bst)
                      if (problems.isEmpty) {
                        System.out.println(s"#OTI/OMF R2 class2concept => RBST OK")
                        val conceptPair = Vector(TboxUMLElementTreeType(Some(tbox), cConcept, bst))
                        RuleResult[Uml, Omf, Provenance](
                          rule,
                          finalResults=conceptPair, // this is a result
                          internalResults=Vector(),
                          externalResults=conceptPair // it needs to be further expanded in the next phase
                        ).right
                      } else {
                        System.out.println(s"#OTI/OMF R2 class2concept => RBST ${problems.size} problems")
                        Set(
                          treeOpsException(
                            context.treeOps,
                            problems
                              .map(_.toString)
                              .mkString(s"TreeCompositeStructure has ${problems.size} problems\n", "\n", "\n"))
                        ).left
                      }
                    case tree =>
                      System.out.println(s"#OTI/OMF R2 class2concept => other $tree")
                      Set(
                        treeOpsException(
                          context.treeOps,
                          s"Not a TreeCompositeStructureType: $tree")
                      ).left
                  }
              else {
                System.out.println(s"#OTI/OMF R2 class2concept => not RBST")
                val conceptPair = Vector(TboxUMLElement2ConceptDefinition(Some(tbox), cConcept, c))
                RuleResult[Uml, Omf, Provenance](
                  rule,
                  finalResults=conceptPair, // this is a result
                  internalResults=Vector(),
                  externalResults=Vector() // @todo enable when there are data property mapping rules conceptPair
                ).right
              }
            }

        rC.flatMap{ _ =>
          result
        }
      }
      .toThese
    def other2concept
    (rule: MappingFunction[Uml, Omf, Provenance],
     tbox: Omf#MutableModelTerminologyGraph,
     cls: UMLClassifier[Uml],
     as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
     cs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap)
    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance] =
      context.mapElement2Concept(rule, tbox, cls, cls.isAbstract)
        .flatMap { cConcept =>
//          System.out
//            .println(s"#OTI/OMF R2 other2concept: ${cls.qualifiedName.get} / a:${as.size}, c:${cs.size}")

          val r0: Set[java.lang.Throwable] \/ Unit = \/-(())
          val rA: Set[java.lang.Throwable] \/ Unit = (r0 /: as) {
            case (ri, (ai, aiOmf)) =>
//              System.out.println(
//                s"""|#OTI/OMF R2 EntityDefinitionAspectSubClassAxiom:
//                    | sup=${ai.name.get}, sub=${cls.qualifiedName.get}""".stripMargin)
              ri +++
                context
                  .addEntityDefinitionAspectSubClassAxiom(rule, tbox, cConcept, aiOmf)
                  .map(_ => ())
          }

          val rC: Set[java.lang.Throwable] \/ Unit = (rA /: cs) {
            case (ri, (ci, ciOmf)) =>
//              System.out.println(
//                s"""|#OTI/OMF R2 EntityConceptSubClassAxiom:
//                    | sup=${ci.name.get}, sub=${cls.qualifiedName.get}""".stripMargin)
              ri +++
                context
                  .addEntityConceptSubClassAxiom(rule, tbox, cConcept, ciOmf)
                  .map(_ => ())
          }

          rC.flatMap{ _ =>
            val conceptPair = Vector(TboxUMLElement2ConceptDefinition(Some(tbox), cConcept, cls))
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults=conceptPair, // this is a result
              internalResults=Vector(),
              externalResults=Vector() // @todo enable when there are data property mapping rules conceptPair
            ).right
          }
        }
      .toThese

    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (rule, TboxUMLElementTuple(Some(tbox), neU: UMLNamedElement[Uml]), as, cs, rs, unmappedS)
        if cs.nonEmpty && rs.isEmpty =>

        val result
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = {
          if (unmappedS.nonEmpty) {
            val foreign = unmappedS.filter(!context.otherStereotypesApplied.contains(_))
            require(foreign.isEmpty)
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
}