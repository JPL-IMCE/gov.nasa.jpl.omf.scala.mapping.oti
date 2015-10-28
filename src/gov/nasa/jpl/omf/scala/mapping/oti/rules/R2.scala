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
case class R2[Uml <: UML, Omf <: OMF]()(implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf]) {

  /**
   * Map an OTI UMLNamespace as an OMF aspect according to the mapping of the stereotypes applied.
   */
  def namespace2AspectMapping(context: OTI2OMFMappingContext[Uml, Omf]) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction = {
      case (rule, TboxUMLElementTuple(Some(tbox), clsU: UMLClassifier[Uml]), as, cs, rs, unmappedS)
        if as.nonEmpty && cs.isEmpty && rs.isEmpty =>

        if (unmappedS.nonEmpty) {
          val foreign = unmappedS.filter(!context.otherStereotypesApplied.contains(_))
          require(foreign.isEmpty)
        }

        for {
          clsOmfAspect <- context.mapElement2Aspect(rule, tbox, clsU)
          _ = {
            //val ok:
            as
              .foreach {
                case (aS, aOmf) =>
                  context.addEntityDefinitionAspectSubClassAxiom(rule, tbox, clsOmfAspect, aOmf)
              }
          }

          pkgContents =
          clsU
                        .ownedElement
                        .filter({
                                  case _: UMLAssociation[Uml] => true
                                  case _: UMLNamespace[Uml]   => false
                                  case _                      => true
                                })

          moreContents = pkgContents.map(TboxUMLElementTuple(Some(tbox), _)) toList
        } yield Tuple3(
          TboxUMLElement2EntityDefinition(Some(tbox), clsOmfAspect, clsU) :: Nil,
          Nil,
          moreContents)
    }

    MappingFunction[Uml, Omf]("namespace2AspectMapping", mapping)

  }

  /**
   * Map an OTI UMLNamedElement as an OMF concept according to the mapping of the stereotypes applied.
   */
  def namedElement2ConceptMapping(context: OTI2OMFMappingContext[Uml, Omf]) = {

    def class2concept
    (rule: MappingFunction[Uml, Omf],
     tbox: Omf#MutableModelTerminologyGraph,
     c: UMLClass[Uml],
     as: OTI2OMFMappingContext[Uml, Omf]#UMLStereotype2EntityAspectMap,
     cs: OTI2OMFMappingContext[Uml, Omf]#UMLStereotype2EntityConceptMap)
    : NonEmptyList[java.lang.Throwable] \/ (OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs, OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs, OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs) =
      context.mapElement2Concept(rule, tbox, c, c.isAbstract)
      .flatMap { cConcept =>
        System.out
          .println(s"#OTI/OMF R2 Element2Concept: ${c.qualifiedName.get} / a:${as.size}, c:${cs.size}")

        val r0: NonEmptyList[java.lang.Throwable] \/ Unit = \/-(())
        val rA: NonEmptyList[java.lang.Throwable] \/ Unit = (r0 /: as) {
          case (ri, (ai, aiOmf)) =>
            System.out.println(
              s"""|#OTI/OMF R2 EntityDefinitionAspectSubClassAxiom:
                  | sup=${ai.name.get}, sub=${c.qualifiedName.get}""".stripMargin)
            ri +++
              context
                .addEntityDefinitionAspectSubClassAxiom(rule, tbox, cConcept, aiOmf)
                .map(_ => ())
        }

        val rC: NonEmptyList[java.lang.Throwable] \/ Unit = (rA /: cs) {
          case (ri, (ci, ciOmf)) =>
            System.out.println(
              s"""|#OTI/OMF R2 EntityConceptSubClassAxiom:
                  | sup=${ci.name.get}, sub=${c.qualifiedName.get}""".stripMargin)
            ri +++
              context
                .addEntityConceptSubClassAxiom(rule, tbox, cConcept, ciOmf)
                .map(_ => ())
        }

        val result
        : NonEmptyList[java.lang.Throwable] \/ (OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs, OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs, OTI2OMFMappingContext[Uml, Omf]#TboxUMLElementPairs)  =
          context.treeOps.isRootBlockSpecificType(c)
            .flatMap { isRBST =>
              analyze(c)(context.treeOps, context.idg)
                .flatMap {
                  case bst: TreeCompositeStructureType[Uml] =>
                    val problems = TreeType.getIllFormedTreeBranchPairs(bst)
                    if (problems.isEmpty) {
                      val one = TboxUMLElementTreeType(Some(tbox), cConcept, bst)
                      Tuple3(
                        one :: Nil, // this is a result
                        Nil,
                        one :: Nil // it needs to be further expanded in the next phase
                      ).right
                    } else
                      NonEmptyList(
                        treeOpsException(
                          context.treeOps,
                          problems
                            .map(_.toString)
                            .mkString(s"TreeCompositeStructure has ${problems.size} problems\n", "\n", "\n"))
                      ).left
                  case tree =>
                    NonEmptyList(
                      treeOpsException(
                        context.treeOps,
                        s"Not a TreeCompositeStructureType: $tree")
                    ).left
                }
            }

        rC.flatMap{ _ =>
          result
        }
      }

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction = {
      case (rule, TboxUMLElementTuple(Some(tbox), neU: UMLNamedElement[Uml]), as, cs, rs, unmappedS)
        if cs.nonEmpty && rs.isEmpty =>

        if (unmappedS.nonEmpty) {
          val foreign = unmappedS.filter(!context.otherStereotypesApplied.contains(_))
          require(foreign.isEmpty)
        }

        neU match {
          case c: UMLClass[Uml] =>
            class2concept(rule, tbox, c, as, cs)
          case _                =>
            NonEmptyList(
              treeOpsException(
                context.treeOps,
                s"R2 is not applicable to: $neU",
                UMLError
                .illegalElementError[Uml, UMLElement[Uml]]("#OTI/OMF R2 Element2Concept not a Class:" +
                  neU.qualifiedName.get + ": " +
                  neU.xmiType.head +
                  s" / a:${as.size}, c:${cs.size}, r:${rs.size}", Iterable(neU)))
            ).left
        }
    }

    MappingFunction[Uml, Omf]("namedElement2ConceptMapping", mapping)

  }
}