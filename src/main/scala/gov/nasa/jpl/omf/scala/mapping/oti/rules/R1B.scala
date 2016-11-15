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
import gov.nasa.jpl.omf.scala.mapping.oti._
import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._
import org.omg.oti.uml.UMLError

import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{StringContext,Unit}
import scala.collection.immutable._
import scalaz._, Scalaz._

case class R1B[Uml <: UML, Omf <: OMF, Provenance]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  def documentPackageMapping
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance) = {

    val mapping
    : OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction
    = {

      case (rule, pair: TBoxOTIDocumentPackageConversion[Uml, Omf], as, cs, rs, unmappedS) => {
        val conversions = R1B.documentPackageConversion(context, pkg2provenance, rule, pair, as, cs, rs, unmappedS)
        conversions
      }
    }

    MappingFunction[Uml, Omf, Provenance]("R1B (documentPackageMapping)", mapping)
  }

}

object R1B {

  def documentPackageConversion
  [Uml <: UML, Omf <: OMF, Provenance]
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance,
   rule: MappingFunction[Uml, Omf, Provenance],
   pair: TBoxOTIDocumentPackageConversion[Uml, Omf],
   as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
   cs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap,
   rs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]])
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = {

    val r0: Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance] =
      \&/.That(RuleResult[Uml, Omf, Provenance](
        rule,
        Vector(),
        Vector(),
        Vector(pair.toContentsConversion())))

    val rN: Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance] =
      ( r0 /: pair.e.nestedPackage.filterNot(context.ignoreCrossReferencedElementFilter(_)) ) { (ri, subPkgU) =>

        ri.flatMap { acc =>

          context
            .lookupProjectAuthorityOrSpecificAppliedStereotype(subPkgU)
            .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
            (nels: Set[java.lang.Throwable]) => {
              java.lang.System.out.println(
                s"-@R1B.package(New): [${subPkgU.xmiElementLabel}] ${subPkgU.qualifiedName.get} - ${nels.head}")
              \&/.Both(
                Set(UMLError.illegalElementException[Uml, UMLPackage[Uml]](
                  s"Error looking up authorities for UML Package",
                  Iterable(pair.e),
                  nels)),
                acc)
            },

            (authorities: Set[UMLStereotype[Uml]]) =>
              if (authorities.isEmpty)
                \&/.That(acc.copy(
                    internalResults =
                      acc.internalResults :+ pair.copy(e = subPkgU)(context.ops))

                )
              else
                context
                  .nestedPackageOrAuthority2TBox(rule, pair, pkg2provenance, authorities, subPkgU)
                    .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                  (nels: Set[java.lang.Throwable]) => {
                    java.lang.System.out.println(
                      s"-@R1A.package(New): [${pair.e.xmiElementLabel}] ${pair.e.qualifiedName.get} - ${nels.head}")
                    \&/.Both(
                      Set(UMLError.illegalElementException[Uml, UMLPackage[Uml]](
                        s"Error creating an OMF terminology graph for a nested UML package or authority",
                        Iterable(pair.e),
                        nels)),
                      acc)

                  },
                  (nestedPair: TBoxOTIDocumentPackageConversion[Uml, Omf]) => {
                    val cp0
                    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
                    = \&/.That(acc.copy(
                      internalResults = acc.internalResults :+ nestedPair)
                    )

                    val cp1
                    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
                    = (cp0 /: nestedPair.e.profileApplication) { (cp_acc, pa) =>
                      cp_acc.flatMap { cpi =>
                        pa
                          .appliedProfile
                          .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]] {

                          \&/.Both(
                            Set(UMLError.illegalElementError[Uml, UMLProfileApplication[Uml]](
                              s"Missing applied profile",
                              Iterable(pa))),
                            cpi)

                        }{ apf =>

                          context
                            .lookupModelTerminologyGraphByProfile(apf)
                            .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]] {

                            \&/.Both(
                              Set(UMLError.illegalElementError[Uml, UMLProfileApplication[Uml]](
                                s"Applied profile does not map to an OMF Terminology graph",
                                Iterable(pa))),
                              cpi)

                          } { appliedProfileTbox =>

                            context
                              .addDirectlyExtendedTerminologyGraph(rule, pa, nestedPair.pkgDocumentTbox, appliedProfileTbox)
                              .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                              (nels: Set[java.lang.Throwable]) =>
                                \&/.Both(
                                  Set(UMLError.illegalElementException[Uml, UMLProfileApplication[Uml]](
                                    s"Error extending OMF Terminology graph according to applied profile",
                                    Iterable(pa),
                                    nels)),
                                  cpi),

                              (_: Unit) =>
                                \&/.That(cpi)
                            )

                          }

                        }

                      }
                    }

                    val cp2
                    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
                    = (cp1 /: nestedPair.e.packageImport) { (cp_acc, pi) =>
                      cp_acc.flatMap { cpi =>
                        pi
                          .importedPackage
                          .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]] {

                          \&/.Both(
                            Set(UMLError.illegalElementError[Uml, UMLPackageImport[Uml]](
                              s"Missing imported package",
                              Iterable(pi))),
                            cpi)

                        }{ ipkg =>

                          context
                            .lookupModelTerminologyGraphByPackage(ipkg)
                            .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]] {

                            \&/.Both(
                              Set(UMLError.illegalElementError[Uml, UMLPackageImport[Uml]](
                                s"Imported package does not map to an OMF Terminology graph",
                                Iterable(pi))),
                              cpi)

                          } { importedPackageTbox =>

                            context
                              .addDirectlyExtendedTerminologyGraph(rule, pi, nestedPair.pkgDocumentTbox, importedPackageTbox)
                              .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                              (nels: Set[java.lang.Throwable]) =>
                                \&/.Both(
                                  Set(UMLError.illegalElementException[Uml, UMLPackageImport[Uml]](
                                    s"Error extending OMF Terminology graph according to imported package",
                                    Iterable(pi),
                                    nels)),
                                  cpi),

                              (_: Unit) =>
                                \&/.That(cpi)
                            )

                          }

                        }

                      }
                    }

                    cp2

                  }
                )
          )
        }
      }

    rN
  }
}