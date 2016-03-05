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

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti._
import org.omg.oti.uml._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{Some,StringContext,Tuple3,Unit}
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
case class R1[Uml <: UML, Omf <: OMF, Provenance]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  /**
    * Mapping of an OTI profile PF to OMF:
    * - Verify that there is already an immutable OMF Tbox corresponding to PF
    *
    * Mapping of an OTI non-profile Package P to OMF:
    * - If no stereotype is applied to P, treat P as if base:Package had been applied.
    *
    * @param context The OTI2OMF Mapping Context
    */
  def profileOrPackageMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    pkg2provenance: UMLPackage[Uml] => Provenance ) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {

      case (rule, TboxUMLElementTuple(Some(tbox), pfU: UMLProfile[Uml]), as, cs, rs, unmappedS) =>
        val result
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = context.lookupImmutableModelTerminologyGraphByProfile(pfU)
          .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]]({

          context.lookupMutableModelTerminologyGraphByProfile(pfU)
            .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]]({
            java.lang.System.out.println(s"-R1.profile: [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get} => no OMF Graph")
            \&/.This(Set(
              UMLError.illegalElementError[Uml, UMLProfile[Uml]](
                s"profile ${pfU.qualifiedName.get} should have been mapped to an immutable OMF graph",
                Iterable(pfU))
            ))
          }) { pfOnt =>
            context.lookupDocumentPackageScopeAndTerminologyGraph(pfU)
              .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
              l = (nels) =>
                \&/.This(nels),
              r = _.fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                \&/.This(Set(
                  UMLError.illegalElementError[Uml, UMLPackage[Uml]](
                    s"Failed to lookup the document for profile: ${pfU.qualifiedName.get}",
                    Iterable(pfU)
                  )
                ))
              ) { case (pfD, pfG) =>

                // The contents of pfU need to be mapped to pfOnt (mutable)
                java.lang.System.out.println(s"+R1.profile(M): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get}")
                val pairs = Vector(TboxUMLProfile2MutableTBoxConversion(pfOnt.some, pfU, pfD, pfG))
                \&/.That(RuleResult[Uml, Omf, Provenance](
                  rule,
                  finalResults = pairs,
                  internalResults = pairs,
                  externalResults = Vector()))
              })
          }
        }) { pfOnt =>
          // The contents of pfU are, by definition, already in pfOnt (immutable)
          java.lang.System.out.println(s"+R1.profile(I): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get}")
          \&/.That(RuleResult[Uml, Omf, Provenance](
            rule,
            finalResults = Vector(TboxUMLProfile2ImmutableTBoxTuple(pfOnt.some, pfU)),
            internalResults = Vector(),
            externalResults = Vector()))
        }

        result

      case (rule, TboxUMLElementTuple(Some(tbox), pkgU: UMLPackage[Uml]), as, cs, rs, unmappedS) =>
        java.lang.System.out.println(s"@R1.package: [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
        val result
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = context.lookupImmutableModelTerminologyGraphByPackage(pkgU)
          .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
          context.lookupMutableModelTerminologyGraphByPackage(pkgU)
            .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]]({

            val errors_or_pkgTbox =
              context
                .ns2tboxCtor(rule, pkgU, TerminologyKind.isToplevelDefinition, pkg2provenance(pkgU))

            errors_or_pkgTbox
              .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
              (nels: Set[java.lang.Throwable]) => {
                java.lang.System.out.println(s"-@R1.package(New): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} - ${nels.head}")
                \&/.This(nels)
              },
              (pkgTbox: Omf#MutableModelTerminologyGraph) => {
                context.lookupDocumentPackageScopeAndTerminologyGraph(pkgU)
                  .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                  l = (nels) =>
                    \&/.This(nels),
                  r = _.fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
                    \&/.This(Set(
                      UMLError.illegalElementError[Uml, UMLPackage[Uml]](
                        s"Failed to lookup the document for package: ${pkgU.qualifiedName.get}",
                        Iterable(pkgU)
                      )
                    ))
                  ) { case (pkgD, pkgG) =>

                    context.partitionAppliedStereotypesByMapping(pkgU)
                      .flatMap { case (pkMappedS, pkgUnmappedS) =>
                        if (pkgUnmappedS.nonEmpty) {
                          val foreign = pkgUnmappedS.filter(!context.otherStereotypesApplied.contains(_))
                          if (foreign.nonEmpty) {
                            java.lang.System.out.println(s"**** ${foreign.size} foreign stereotypes applied!")
                            foreign.foreach { s =>
                              java.lang.System.out.println(s"**** foreign: ${s.qualifiedName.get}")

                            }
                          }
                        }

                        val superConcepts =
                          if (pkMappedS.isEmpty) Set(context.basePackageC)
                          else pkMappedS.map(context.stereotype2Concept(_))

                        context.mapElement2Concept(rule, pkgTbox, pkgU, isAbstract = false)
                          .flatMap { pkgC =>

                            // The contents of pkgU need to be mapped to pkgOnt (mutable)
                            val pairs =
                              Vector(TboxUMLPackage2MutableTBoxConversion(pkgTbox.some, pkgU, pkgD, pkgG, pkgC, superConcepts))
                            java.lang.System.out.println(s"+@R1.package(New)O ${context.ops.getTerminologyGraphIRI(pkgTbox)}")
                            RuleResult[Uml, Omf, Provenance](
                              rule,
                              finalResults = pairs,
                              internalResults = pairs,
                              externalResults = Vector()
                            ).right
                          }
                      }.toThese
                  })
              })
          }) { pkgOnt =>
            // @todo Is this case possible at all?
            java.lang.System.out.println(s"+R1.package(M): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
            \&/.That(RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults = Vector(TboxUMLPackage2MutableTBoxTuple(pkgOnt.some, pkgU)),
              internalResults = Vector(),
              externalResults = Vector()
            ))
          }) { pkgOnt =>
          // The contents of pkgU are, by definition, already in pkgOnt (immutable)
          java.lang.System.out.println(s"+R1.package(I): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
          \&/.That(RuleResult[Uml, Omf, Provenance](
            rule,
            finalResults = Vector(TboxUMLPackage2ImmutableTBoxTuple(pkgOnt.some, pkgU)),
            internalResults = Vector(),
            externalResults = Vector()
          ))
        }

        result

      case (rule,
      TboxUMLPackage2MutableTBoxConversion(Some(pkgTbox), pkgU, pkgOTIDocument, pkgDocumentTBox, pkgC, superConcepts),
      as, cs, rs, unmappedS) =>
        java.lang.System.out
          .println(
            s"#OTI/OMF R1 pkgConversion: ${pkgU.qualifiedName.get} (${superConcepts.size} pkg concept classifications)")

        val step0: Set[java.lang.Throwable] \/ Unit = \/-(())

        val step1: Set[java.lang.Throwable] \/ Unit = {
          val pfApplications = pkgU.profileApplication
          if (pfApplications.isEmpty) {
            val step1a
            : Set[java.lang.Throwable] \/ Unit
            = (step0 /: context.pf2ont) { case (acc, (pU, pO)) =>
              acc.flatMap { _ =>
                java.lang.System.out.println(
                  s"#OTI/OMF R1 pkgConversion: ${pkgU.qualifiedName.get} (implicitly applying ${pU.qualifiedName.get}")
                context.addDirectlyExtendedTerminologyGraph(rule, pkgTbox, pO).map(_ => ())
              }
            }
            step1a
          } else {
            val step1b
            : Set[java.lang.Throwable] \/ Unit
            = (step0 /: pfApplications) {
              (acc, pfApplication) =>
                acc.flatMap { _ =>
                  val apfU = pfApplication.appliedProfile.get // @todo check for no applied profile
                  context
                    .lookupDocumentPackageScopeAndTerminologyGraph(apfU)
                    .fold[Set[java.lang.Throwable] \/ Unit](
                    l = (nels) => nels.left,
                    r = _.fold[Set[java.lang.Throwable] \/ Unit](
                      Set(
                        UMLError.illegalElementError[Uml, UMLProfile[Uml]](
                          s"document/graph missing for applied profile: ${apfU.qualifiedName.get}",
                          Iterable(apfU)
                        )
                      ).left
                    ) { case (apfDocument, apfTbox) =>
                      context.addDirectlyExtendedTerminologyGraph(rule, pkgTbox, apfTbox).map(_ => ())
                    })
                }
            }
            step1b
          }
        }

        val step2: Set[java.lang.Throwable] \/ Unit = {
          val pkgImports = pkgU.packageImport
          if (pkgImports.isEmpty) {
            val step2a
            : Set[java.lang.Throwable] \/ Unit
            = (step0 /: context.pkg2ont) { case (acc, (pU, pO)) =>
              acc.flatMap { _ =>
                java.lang.System.out.println(
                  s"#OTI/OMF R1 pkgConversion: ${pkgU.qualifiedName.get} (implicitly importing ${pU.qualifiedName.get}")
                context.addDirectlyExtendedTerminologyGraph(rule, pkgTbox, pO).map(_ => ())
              }
            }
            step2a
          } else {
            val step2b
            : Set[java.lang.Throwable] \/ Unit
            = (step1 /: pkgImports) { (acc, pkgImport) =>
                acc.flatMap { _ =>
                  val ipkgU = pkgImport.importedPackage.get // @todo check for no imported package
                  context.lookupDocumentPackageScopeAndTerminologyGraph(ipkgU)
                    .fold[Set[java.lang.Throwable] \/ Unit](
                    l = (nels) => nels.left,
                    r = _.fold[Set[java.lang.Throwable] \/ Unit]({
                      java.lang.System.out
                        .println(
                          s"#OTI/OMF R1 pkgConversion: ${pkgU.qualifiedName.get} "+
                          s"imported package not mapped to a document: ${ipkgU.qualifiedName.get}")
                      \/-(())
                    }) { case (ipkgDocument, ipkgTbox) =>
                      context.addDirectlyExtendedTerminologyGraph(rule, pkgTbox, ipkgTbox).map(_ => ())
                    })
                }
            }
            step2b
          }
        }

        val s0: Set[java.lang.Throwable] \/ Unit = step2
        val sN: Set[java.lang.Throwable] \/ Unit = (s0 /: superConcepts) { (si, supC) =>
          si +++
            context
              .addEntityConceptSubClassAxiom(rule, pkgTbox, pkgC, supC)
              .map(_ => ())
        }
        val sResult = sN.flatMap { _ =>
          java.lang.System.out
            .println(s"#OTI/OMF R1 pkg2concept: lookup...")

            // owned UML elements to map in the subsequent content phase
            val pkgContents: Set[UMLElement[Uml]] =
              pkgOTIDocument
              .extent.filter {
                case _: UMLProfile[Uml] =>
                  false
                case _: UMLPackage[Uml] =>
                  true
                case _: UMLPackageImport[Uml] =>
                  false
                case _: UMLElementImport[Uml] =>
                  false
                case _: UMLPackageMerge[Uml] =>
                  false
                case _ =>
                  true
              }
              .to[Set]

            java.lang.System.out.println(
              s"#OTI/OMF R1 pkgConversion: [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} "+
              s"=> ${pkgContents.size} contents")
            val moreContents = pkgContents.map(TboxUMLElementTuple(pkgTbox.some, _)).toVector

            val pkgPair = Vector(TboxUMLPackage2ConceptDefinition(pkgTbox.some, pkgC, pkgU))
          RuleResult[Uml, Omf, Provenance](
            rule,
            finalResults = pkgPair,
            internalResults = moreContents,
            externalResults = Vector()).right
        }
        sResult.toThese

      case (rule,
      TboxUMLProfile2MutableTBoxConversion(Some(pfTbox), pfU, pfOTIDocument, pfDocumentTBox),
      as, cs, rs, unmappedS) =>
        java.lang.System.out.println(
          s"#OTI/OMF R1 pfConversion: ${pfU.qualifiedName.get}")

        val step0: Set[java.lang.Throwable] \/ Unit = \/-(())

        val step1: Set[java.lang.Throwable] \/ Unit = {
          val pfApplications = pfU.profileApplication
          if (pfApplications.isEmpty) {
            val step1a
            : Set[java.lang.Throwable] \/ Unit
            = (step0 /: context.pf2ont) { case (acc, (pU, pO)) =>
              acc.flatMap { _ =>
                java.lang.System.out.println(
                  s"#OTI/OMF R1 pfConversion: ${pfU.qualifiedName.get} (implicitly applying ${pU.qualifiedName.get}")
                context.addDirectlyExtendedTerminologyGraph(rule, pfTbox, pO).map(_ => ())
              }
            }
            step1a
          } else {
            val step1b
            : Set[java.lang.Throwable] \/ Unit
            = (step0 /: pfApplications) {
              (acc, pfApplication) =>
                acc.flatMap { _ =>
                  val apfU = pfApplication.appliedProfile.get // @todo check for no applied profile
                  context.lookupDocumentPackageScopeAndTerminologyGraph(apfU)
                    .fold[Set[java.lang.Throwable] \/ Unit](
                    l = (nels) => nels.left,
                    r = _.fold[Set[java.lang.Throwable] \/ Unit](
                      Set(
                        UMLError.illegalElementError[Uml, UMLProfile[Uml]](
                          s"document/graph missing for applied profile: ${apfU.qualifiedName.get}",
                          Iterable(apfU)
                        )
                      ).left
                    ) { case (apfDocument, apfTbox) =>
                      context.addDirectlyExtendedTerminologyGraph(rule, pfTbox, apfTbox).map(_ => ())
                    })
                }
            }
            step1b
          }
        }

        val step2: Set[java.lang.Throwable] \/ Unit = {
          val pfImports = pfU.packageImport
          if (pfImports.isEmpty) {
            val step2a
            : Set[java.lang.Throwable] \/ Unit
            = (step0 /: context.pkg2ont) { case (acc, (pU, pO)) =>
              acc.flatMap { _ =>
                java.lang.System.out.println(
                  s"#OTI/OMF R1 pfConversion: ${pfU.qualifiedName.get} (implicitly importing ${pU.qualifiedName.get}")
                context.addDirectlyExtendedTerminologyGraph(rule, pfTbox, pO).map(_ => ())
              }
            }
            step2a
          } else {
            val step2b
            : Set[java.lang.Throwable] \/ Unit
            = (step1 /: pfImports) { (acc, pkgImport) =>
              acc.flatMap { _ =>
                val ipkgU = pkgImport.importedPackage.get // @todo check for no imported package
                context.lookupDocumentPackageScopeAndTerminologyGraph(ipkgU)
                  .fold[Set[java.lang.Throwable] \/ Unit](
                  l = (nels) => nels.left,
                  r = _.fold[Set[java.lang.Throwable] \/ Unit]({
                    java.lang.System.out
                      .println(
                        s"#OTI/OMF R1 pfConversion: ${pfU.qualifiedName.get} "+
                        s"imported package not mapped to a document: ${ipkgU.qualifiedName.get}")
                    \/-(())
                  }) { case (ipkgDocument, ipkgTbox) =>
                    context.addDirectlyExtendedTerminologyGraph(rule, pfTbox, ipkgTbox).map(_ => ())
                  })
              }
            }
            step2b
          }
        }

        val result
        : Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]
        = step2.flatMap { _ =>

          // owned UML elements to map in the subsequent content phase
          val pkgContents: Set[UMLElement[Uml]] = pfOTIDocument.extent.filter({
            case _: UMLProfile[Uml] =>
              false
            case _: UMLPackage[Uml] =>
              true
            case _: UMLPackageImport[Uml] =>
              false
            case _: UMLElementImport[Uml] =>
              false
            case _: UMLPackageMerge[Uml] =>
              false
            case _ =>
              true
          }).to[Set]

          java.lang.System.out
            .println(
              s"#OTI/OMF R1 pfConversion: [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get} "+
              s"=> ${pkgContents.size} contents")
          val moreContents = pkgContents.map(TboxUMLElementTuple(pfTbox.some, _)).toVector

          RuleResult[Uml, Omf, Provenance](
            rule,
            finalResults=Vector(),
            internalResults=moreContents,
            externalResults=Vector()).right
        }

        result.toThese
    }

    MappingFunction[Uml, Omf, Provenance]( "profileOrPackageMapping", mapping )

  }
}