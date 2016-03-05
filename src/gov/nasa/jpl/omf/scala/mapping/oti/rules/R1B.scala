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
import org.omg.oti.uml.UMLError

import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{Some,StringContext,Tuple3,Unit}
import scala.Predef.require
import scala.collection.immutable._
import scala.language.postfixOps
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
      \&/.That(RuleResult[Uml, Omf, Provenance](rule, Vector(), Vector(), Vector()))

    val rN: Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance] =
      ( r0 /: pair.e.nestedPackage ) { (ri, subPkgU) =>

        ri.flatMap { acc =>

          context
            .lookupProjectAuthorityOrSpecificAppliedStereotype(subPkgU)
            .fold[Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]](
            (nels: Set[java.lang.Throwable]) =>
              \&/.Both(nels, acc),

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
                  (nels: Set[java.lang.Throwable]) =>
                    \&/.Both(nels, acc),
                  (nestedPair: TBoxOTIDocumentPackageConversion[Uml, Omf]) =>
                    \&/.That(acc.copy(
                      internalResults = acc.internalResults :+ nestedPair)
                    )
                )
          )
        }
      }

    rN
  }
}