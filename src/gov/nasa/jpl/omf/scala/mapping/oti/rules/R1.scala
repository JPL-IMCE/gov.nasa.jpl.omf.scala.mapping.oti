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

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{Some,StringContext,Tuple3,Unit}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._, Scalaz._

/**
 * Mapping for a kind of UML Package (but not a Profile)
 * 
 * The mapping of a UML Package distinguishes 2 kinds of owned elements:
 * - nested packages will be recursively mapped
 * - non-package owned elements will be mapped in the subsequent phase
 * 
 * For the IMCE authorization pattern, a UML Package, as a kind of UML Namespace,
 * should map to an OMF TerminologyGraph.
 *
 * Currently, this rule does not map a UML Package according to the IMCE authorization pattern.
 */
case class R1[Uml <: UML, Omf <: OMF]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  def r1Error
  (message: String, e: UMLElement[Uml])
  (nels: NonEmptyList[java.lang.Throwable])
  : NonEmptyList[java.lang.Throwable] =
    NonEmptyList(
      UMLError.illegalElementException[Uml, UMLElement[Uml]](message, Iterable(e), nels)
    )

  /**
    * Mapping of an OTI profile PF to OMF:
    * - Verify that there is already an immutable OMF Tbox corresponding to PF
    *
    * Mapping of an OTI non-profile Package P to OMF:
    * - If no stereotype is applied to P, treat P as if base:Package had been applied.
    *
    * @param context The OTI2OMF Mapping Context
    */
  def profileOrPackageMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction = {

      case (rule, TboxUMLElementTuple(Some(tbox), pfU: UMLProfile[Uml]), as, cs, rs, unmappedS) =>
        context.lookupImmutableModelTerminologyGraphByProfile(pfU)
          .fold[NonEmptyList[java.lang.Throwable] \/ OTI2OMFMapper[Uml, Omf]#RulesResult]({

          context.lookupMutableModelTerminologyGraphByProfile(pfU)
            .fold[NonEmptyList[java.lang.Throwable] \/ OTI2OMFMapper[Uml, Omf]#RulesResult]({
            java.lang.System.out.println(s"-R1.profile: [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get} => no OMF Graph")
            NonEmptyList(
              UMLError.illegalElementError[Uml, UMLProfile[Uml]](
                s"profile ${pfU.qualifiedName.get} should have been mapped to an immutable OMF graph",
                Iterable(pfU))
            ).left
          }) { pfOnt =>
            java.lang.System.out.println(s"+R1.profile(M): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get} => $pfOnt")
            Tuple3(
              TboxUMLProfile2MutableTBoxTuple(pfOnt.some, pfU) :: Nil,
              Nil,
              Nil
            ).right
          }
        }) { pfOnt =>
          java.lang.System.out.println(s"+R1.profile(I): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get} => $pfOnt")
          Tuple3(
            TboxUMLProfile2ImmutableTBoxTuple(pfOnt.some, pfU) :: Nil,
            Nil,
            Nil
          ).right
        }

      case (rule, TboxUMLElementTuple(Some(tbox), pkgU: UMLPackage[Uml]), as, cs, rs, unmappedS) =>
        java.lang.System.out.println(s"@R1.package: [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
        context.lookupImmutableModelTerminologyGraphByPackage(pkgU)
          .fold[NonEmptyList[java.lang.Throwable] \/ OTI2OMFMapper[Uml, Omf]#RulesResult](
          context.lookupMutableModelTerminologyGraphByPackage(pkgU)
            .fold[NonEmptyList[java.lang.Throwable] \/ OTI2OMFMapper[Uml, Omf]#RulesResult]({

            context.partitionAppliedStereotypesByMapping(pkgU)
              .flatMap { case (pkMappedS, pkgUnmappedS) =>
                if (pkgUnmappedS.nonEmpty) {
                  val foreign = pkgUnmappedS.filter(!context.otherStereotypesApplied.contains(_))
                  require(foreign.isEmpty)
                }

                val mappedC =
                  if (pkMappedS.isEmpty) Set(context.basePackageC)
                  else pkMappedS.map(context.stereotype2Concept(_))

                context.mapElement2Concept(rule, tbox, pkgU, isAbstract = false)
                  .flatMap { pkgC =>
                    java.lang.System.out
                      .println(s"#OTI/OMF R1 pkg2concept: [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")

                    val s0: NonEmptyList[java.lang.Throwable] \/ Unit = \/-(())
                    val sN: NonEmptyList[java.lang.Throwable] \/ Unit = (s0 /: mappedC) { (si, supC) =>
                      si +++
                        context
                          .addEntityConceptSubClassAxiom(rule, tbox, pkgC, supC)
                          .map(_ => ())
                    }
                    sN.flatMap { _ =>

                      val pkgNested: Set[UMLPackage[Uml]] = pkgU.nestedPackage.filter({
                        case _: UMLProfile[Uml] => false
                        case _ => true
                      })

                      val morePairs = pkgNested.map(TboxUMLElementTuple(tbox.some, _)).toList

                      // owned UML elements to map in the subsequent content phase
                      val pkgContents: Set[UMLElement[Uml]] = pkgU.ownedElement.filter({
                        case _: UMLPackage[Uml] => false
                        case _ => true
                      })

                      val moreContents = pkgContents.map(TboxUMLElementTuple(tbox.some, _)).toList

                      val pkgPair = TboxUMLPackage2ConceptDefinition(tbox.some, pkgC, pkgU) :: Nil

                      Tuple3(
                        pkgPair,
                        morePairs,
                        moreContents).right
                    }
                  }
              }
          }) { pkgOnt =>
            java.lang.System.out.println(s"+R1.package(M): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} => $pkgOnt")
            Tuple3(
              TboxUMLPackage2MutableTBoxTuple(pkgOnt.some, pkgU) :: Nil,
              Nil,
              Nil
            ).right
          }) { pkgOnt =>
          java.lang.System.out.println(s"+R1.package(I): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} => $pkgOnt")
          Tuple3(
            TboxUMLPackage2ImmutableTBoxTuple(pkgOnt.some, pkgU) :: Nil,
            Nil,
            Nil
          ).right
        }

    }

    MappingFunction[Uml, Omf]( "profileOrPackageMapping", mapping )

  }
}