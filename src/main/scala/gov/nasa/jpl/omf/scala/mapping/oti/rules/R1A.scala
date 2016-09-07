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

import org.omg.oti.uml._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._
import org.omg.oti.uml.xmi.BuiltInDocument

import scala.{StringContext,Unit}
import scala.Predef.require
import scala.collection.immutable._
import scalaz._, Scalaz._

/**
  * Toplevel mapping of a kind of UML Package with an OTI Document.
  */
case class R1A[Uml <: UML, Omf <: OMF, Provenance]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  def documentMapping
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance) = {

    val mapping
    : OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction
    = {

      case (rule, pair: TBoxOTIDocumentProfilePair[Uml, Omf], as, cs, rs, unmappedS) =>
        R1A.profileDocumentMapping(context, pkg2provenance, rule, pair, as, cs, rs, unmappedS)

      case (rule, pair: TBoxOTIDocumentPackagePair[Uml, Omf], as, cs, rs, unmappedS) =>
        R1A.packageDocumentMapping(context, pkg2provenance, rule, pair, as, cs, rs, unmappedS)

    }

    MappingFunction[Uml, Omf, Provenance]("R1A (documentMapping)", mapping)
  }

}

object R1A {

  /**
    * maps a TBoxOTIDocumentProfilePair(pfU, pfD)
    *
    * @param context        OTI Mapping Context
    * @param pkg2provenance Provenange generator
    * @param rule           Mapping rule
    * @param pair           TBoxOTIDocumentProfilePair(pfU, pfD) to be mapped
    * @param as             aspect-mapped stereotypes applied
    * @param cs             concet-mapped stereotypes applied
    * @param rs             reified relationship-mapped stereotypes applied
    * @param unmappedS      unmapped stereotypes
    * @return Find pfG, the OMF TBox corresponding to pfU
    *         - Error: (no pfG)
    *         - finalResult:
    *         TBoxOTIDocumentProfileConversion(pfU, pfD, pfG) if pfG is mutable
    *         The contents have been generated by the steroetype specialization analysis
    *         TBoxOTIDocumentProfileConverted(pfU, pdD, pfG) if pfG is immutable
    *         The contents have been loaded from OMF/OWL.
    */
  def profileDocumentMapping
  [Uml <: UML, Omf <: OMF, Provenance]
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance,
   rule: MappingFunction[Uml, Omf, Provenance],
   pair: TBoxOTIDocumentProfilePair[Uml, Omf],
   as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
   cs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap,
   rs: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]])
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = if (pair.authorities.nonEmpty)
      \&/.This(Set[java.lang.Throwable](

        UMLError.illegalElementError[Uml, UMLProfile[Uml]](
          s"${pair.authorities.size} project:Authority-based stereotypes " +
            s"applied to profile ${pair.e.qualifiedName.get} (there should be zero!) " +
            pair.authorities.map(_.qualifiedName.get).toList.sorted.mkString("\n<<", ">>\n<<", ">>"),
          Iterable(pair.e)
        )
      ))
    else {
      val pfU: UMLProfile[Uml] = pair.e
      val result
      : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
      = context.lookupImmutableModelTerminologyGraphByProfile(pfU)
        .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]] {

        context.lookupMutableModelTerminologyGraphByProfile(pfU)
          .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]] {

          require(!context.specializingProfiles.contains(pfU))

          java.lang.System.out.println(
            s"-R1A.profile: [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get} => " +
              s"cannot be mapped as an extension of an OMF-mapped profile")
          \&/.This(Set(
            UMLError.illegalElementError[Uml, UMLProfile[Uml]](
              s"R1A: profile ${pfU.qualifiedName.get} cannot be mapped as an extension of an OMF-mapped profile",
              Iterable(pfU))
          ))
        } { pfOnt =>

          // The contents of pfU has been already mapped to pfOnt (mutable)
          // by the stereotype specialization analysis; nothing else to do.
          java.lang.System.out.println(
            s"+R1A.profile(M): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get}")
          val pairs = Vector(pair.toConversion(pfOnt))
          \&/.That(RuleResult[Uml, Omf, Provenance](
            rule,
            finalResults = pairs,
            internalResults = Vector(),
            externalResults = Vector()
          ))

        }
      } { pfOnt =>
        // The contents of pfU are, by definition, already in pfOnt (immutable)
        // nothing else to do.
        java.lang.System.out.println(
          s"+R1A.profile(I): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get}")
        \&/.That(RuleResult[Uml, Omf, Provenance](
          rule,
          finalResults = Vector(pair.toConverted(pfOnt)),
          internalResults = Vector(),
          externalResults = Vector()
        ))
      }

      result
    }

  /**
    * maps a TBoxOTIDocumentPackagePair(pkgU, pkgD)
    *
    * @param context OTI Mapping Context
    * @param pkg2provenance Provenange generator
    * @param rule Mapping rule
    * @param pair TBoxOTIDocumentPackagePair(pkgU, pkgD) to be mapped
    * @param as aspect-mapped stereotypes applied
    * @param cs concet-mapped stereotypes applied
    * @param rs reified relationship-mapped stereotypes applied
    * @param unmappedS unmapped stereotypes
    * @return Find pkgG, the OMF TBox corresponding to pkgU
    *         - Error: pkgG is mutable
    *           pfU cannot be mapped as an extension of an OMF-mapped profile
    *         - externalResult: (no pkgG)
    *           TBoxOTIDocumentPackageConversion(pkgU, pkgD, pkgG), pkgG created
    *         - finalResult:
    *           TBoxOTIDocumentPackageConverted(pkgU, pkgD, pkgG) if pkgG is immutable
    */
  def packageDocumentMapping
  [Uml <: UML, Omf <: OMF, Provenance]
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance,
   rule: MappingFunction[Uml, Omf, Provenance],
   pair: TBoxOTIDocumentPackagePair[Uml, Omf],
   as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
   cs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap,
   rs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]])
  : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
  = {

    val pkgU: UMLPackage[Uml] = pair.e

    java.lang.System.out.println(s"@R1A.package: [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
    val result
    : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
    = context.lookupImmutableModelTerminologyGraphByPackage(pkgU)
      .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]]{

      context.lookupMutableModelTerminologyGraphByPackage(pkgU)
        .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]] {

        pair.pkgOTIDocument match {

          case _: BuiltInDocument =>

            java.lang.System.out.println(
              s"-@R1A.package(BuiltIn): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} - " +
                s" ${pair.pkgOTIDocument.info.packageURI}")
            \&/.That(RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults = Vector(),
              internalResults = Vector(),
              externalResults = Vector()
            ))

          case _ =>

            java.lang.System.out.println(
              s"+R1A.package(Doc): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} => new OMF Graph")

            context
              .packageOrAuthority2TBox(rule, pair, pkg2provenance)
              .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
              (nels: Set[java.lang.Throwable]) => {
                java.lang.System.out.println(
                  s"-@R1A.package(New): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} - ${nels.head}")
                \&/.This(
                  Set(UMLError.illegalElementException[Uml, UMLPackage[Uml]](
                    s"Error creating an OMF terminology graph for a UML package or authority",
                    Iterable(pair.e),
                    nels)))
              },
              (convertedPair: TBoxOTIDocumentPackageConversion[Uml, Omf]) => {
                val cp0
                : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
                = \&/.That(RuleResult[Uml, Omf, Provenance](
                  rule,
                  finalResults = Vector(),
                  internalResults = Vector(),
                  externalResults = Vector(convertedPair)
                ))

                val cp1
                : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
                = (cp0 /: convertedPair.e.profileApplication) { (acc, pa) =>
                  acc.flatMap { cpi =>
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
                          .addDirectlyExtendedTerminologyGraph(rule, convertedPair.pkgDocumentTbox, appliedProfileTbox)
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
                = (cp1 /: convertedPair.e.packageImport) { (acc, pi) =>
                  acc.flatMap { cpi =>
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
                          .addDirectlyExtendedTerminologyGraph(rule, convertedPair.pkgDocumentTbox, importedPackageTbox)
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
        }
      } { pkgTbox =>

        \&/.This(Set(
          UMLError.illegalElementError[Uml, UMLPackage[Uml]](
            s"R1A: package ${pkgU.qualifiedName.get} should have not yet been mapped"+
            s"to a mutable OMF TBox: ${context.ops.getTerminologyGraphIRI(pkgTbox)}",
            Iterable(pkgU))
        ))

      }
    } { pkgTbox =>
      // The contents of pkgU are, by definition, already in pkgOnt (immutable)
      java.lang.System.out.println(
        s"+R1A.package(I): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
      \&/.That(RuleResult[Uml, Omf, Provenance](
        rule,
        finalResults = Vector(pair.toConverted(pkgTbox)),
        internalResults = Vector(),
        externalResults = Vector()
      ))
    }

    result
  }

}