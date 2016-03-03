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
import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

import org.omg.oti.uml._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{Some,StringContext,Tuple3,Unit}
import scala.Predef.require
import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._, Scalaz._

/**
  * Toplevel mapping of a kind of UML Package with an OTI Document.
  */
case class R1A[Uml <: UML, Omf <: OMF, Provenance]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  def documentMapping
  ( context: OTI2OMFMappingContext[Uml, Omf, Provenance],
    pkg2provenance: UMLPackage[Uml] => Provenance ) = {

    val mapping
    : OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction
    = {

      case (rule, pair: TBoxOTIDocumentProfilePair[Uml, Omf], as, cs, rs, unmappedS) =>
        profileDocumentMapping(context, pkg2provenance, rule, pair, as, cs, rs, unmappedS)

      case (rule, pair: TBoxOTIDocumentPackagePair[Uml, Omf], as, cs, rs, unmappedS) =>
        packageDocumentMapping(context, pkg2provenance, rule, pair, as, cs, rs, unmappedS)

    }

    MappingFunction[Uml, Omf, Provenance]( "R1A (documentMapping)", mapping )
  }

  /**
    * maps a TBoxOTIDocumentProfilePair(pfU, pfD)
    *
    * @param context OTI Mapping Context
    * @param pkg2provenance Provenange generator
    * @param rule Mapping rule
    * @param pair TBoxOTIDocumentProfilePair(pfU, pfD) to be mapped
    * @param as aspect-mapped stereotypes applied
    * @param cs concet-mapped stereotypes applied
    * @param rs reified relationship-mapped stereotypes applied
    * @param unmappedS unmapped stereotypes
    * @return Find pfG, the OMF TBox corresponding to pfU
    *         - Error: (no pfG)
    *           pfU cannot be mapped as an extension of an OMF-mapped profile
    *         - externalResult:
    *           TBoxOTIDocumentProfileConversion(pfU, pfD, pfG) if pfG is mutable
    *         - finalResult:
    *           TBoxOTIDocumentProfileConverted(pfU, pdD, pfG) if pfG is immutable
    */
  def profileDocumentMapping
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance,
   rule: MappingFunction[Uml, Omf, Provenance],
   pair: TBoxOTIDocumentProfilePair[Uml, Omf],
   as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
   cs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap,
   rs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]])
  : Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]
  = context
    .lookupProjectAuthorityOrSpecificAppliedStereotype(pair.e)
    .flatMap { projectAuthoritiesApplied =>

      if (projectAuthoritiesApplied.nonEmpty)

        -\/(Set[java.lang.Throwable](
          UMLError.illegalElementError[Uml, UMLProfile[Uml]](
            s"${projectAuthoritiesApplied.size} project:Authority-based stereotypes " +
              s"applied to profile ${pair.e.qualifiedName.get} (there should be zero!) " +
              projectAuthoritiesApplied.map(_.qualifiedName.get).toList.sorted.mkString("\n<<", ">>\n<<", ">>"),
            Iterable(pair.e)
          )
        ))

      else {
        val pfU: UMLProfile[Uml] = pair.e
        val result
        : Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]
        = context.lookupImmutableModelTerminologyGraphByProfile(pfU)
          .fold[Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]] {

          context.lookupMutableModelTerminologyGraphByProfile(pfU)
            .fold[Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]] {

            require(!context.specializingProfiles.contains(pfU))

            java.lang.System.out.println(
              s"-R1A.profile: [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get} => " +
                s"cannot be mapped as an extension of an OMF-mapped profile")
            Set(
              UMLError.illegalElementError[Uml, UMLProfile[Uml]](
                s"R1A: profile ${pfU.qualifiedName.get} cannot be mapped as an extension of an OMF-mapped profile",
                Iterable(pfU))
            ).left
          } { pfOnt =>

            // The contents of pfU need to be mapped to pfOnt (mutable)
            // The mapping is deferred to a subsequent phase (external results)
            java.lang.System.out.println(
              s"+R1A.profile(M): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get}")
            val pairs = pair.toConversion(pfOnt) :: Nil
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults = pairs,
              internalResults = Nil,
              externalResults = pairs
            ).right

          }
        } { pfOnt =>
          // The contents of pfU are, by definition, already in pfOnt (immutable)
          // nothing else to do.
          java.lang.System.out.println(
            s"+R1A.profile(I): [${pfU.xmiElementLabel}] ${pfU.qualifiedName.get}")
          RuleResult[Uml, Omf, Provenance](
            rule,
            finalResults = pair.toConverted(pfOnt) :: Nil,
            internalResults = Nil,
            externalResults = Nil
          ).right
        }

        result
      }
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
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance,
   rule: MappingFunction[Uml, Omf, Provenance],
   pair: TBoxOTIDocumentPackagePair[Uml, Omf],
   as: OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityAspectMap,
   cs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityConceptMap,
   rs:  OTI2OMFMappingContext[Uml, Omf, Provenance]#UMLStereotype2EntityRelationshipMap,
   unmappedS: Set[UMLStereotype[Uml]])
  : Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]
  = {

    val pkgU: UMLPackage[Uml] = pair.e

    java.lang.System.out.println(s"@R1A.package: [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
    val result
    : Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]
    = context.lookupImmutableModelTerminologyGraphByPackage(pkgU)
      .fold[Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]]{

      context.lookupMutableModelTerminologyGraphByPackage(pkgU)
        .fold[Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]]{

        java.lang.System.out.println(
          s"+R1A.package(Doc): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} => new OMF Graph")

        val errors_or_pkgTbox =
          context
            .ns2tboxCtor(rule, pkgU, TerminologyKind.isToplevelDefinition, pkg2provenance(pkgU))

        errors_or_pkgTbox
          .fold[Set[java.lang.Throwable] \/ RuleResult[Uml, Omf, Provenance]](
          (nels: Set[java.lang.Throwable]) => {
            java.lang.System.out.println(
              s"-@R1A.package(New): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get} - ${nels.head}")
            -\/(nels)
          },
          (pkgTbox: Omf#MutableModelTerminologyGraph) => {
            RuleResult[Uml, Omf, Provenance](
              rule,
              finalResults = Nil,
              internalResults = pair.toConversion(pkgTbox) :: Nil,
              externalResults = Nil
            ).right
          })
      } { pkgTbox =>

        Set(
          UMLError.illegalElementError[Uml, UMLPackage[Uml]](
            s"R1A: package ${pkgU.qualifiedName.get} should have not yet been mapped"+
            s"to a mutable OMF TBox: ${context.ops.getTerminologyGraphIRI(pkgTbox)}",
            Iterable(pkgU))
        ).left

      }
    } { pkgTbox =>
      // The contents of pkgU are, by definition, already in pkgOnt (immutable)
      java.lang.System.out.println(
        s"+R1A.package(I): [${pkgU.xmiElementLabel}] ${pkgU.qualifiedName.get}")
      RuleResult[Uml, Omf, Provenance](
        rule,
        finalResults = pair.toConverted(pkgTbox) :: Nil,
        internalResults = Nil,
        externalResults = Nil
      ).right
    }

    result
  }


}