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

import scala.language.postfixOps
import scala.util.Success

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

  /**
   * Map an OTI non-profile Package P to OMF
   * If no stereotype is applied to P, treat P as if base:Package had been applied.
   */
  def nonProfilePackageMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxUMLElementTuple( Some( tbox ), pkgU: UMLPackage[Uml] ), as, cs, rs, unmappedS ) =>
          require( oclIsTypeOfPackage( pkgU ) )

          val ( mappedS, unmappedS ) = context.partitionAppliedStereotypesByMapping( pkgU )
          if ( unmappedS.nonEmpty ) {
            val foreign = unmappedS.filter( !context.otherStereotypesApplied.contains( _ ) )
            require ( foreign.isEmpty )
          }

          val mappedC =
            if ( mappedS.isEmpty ) Set( context.basePackageC )
            else mappedS.map( context.stereotype2Concept( _ ) )

          for {
            pkgTbox <- context.ns2tboxCtor(rule, pkgU, TerminologyKind.isDefinition)
            _ = context.addDirectlyNestedTerminologyGraph(rule, tbox, pkgTbox)

            pkgNested = pkgU.nestedPackage.filter({
              case _: UMLProfile[Uml] => false
              case _ => true
            })

            morePairs = pkgNested.map(TboxUMLElementTuple(Some(pkgTbox), _))

            // owned UML elements to map in the subsequent content phase
            pkgContents = pkgU.ownedElement.filter({
              case _: UMLPackage[Uml] => false
              case _ => true
            })

            moreContents = pkgContents.map(TboxUMLElementTuple(Some(pkgTbox), _))
          } yield Tuple2( morePairs ++ moreContents toList, Nil )
      }

    MappingFunction[Uml, Omf]( "nonProfilePackackageMapping", mapping )

  }
}