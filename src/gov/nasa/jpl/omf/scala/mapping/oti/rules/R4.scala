/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.omf.scala.mapping.oti.rules

import org.omg.oti._
import org.omg.oti.api._
import org.omg.oti.api.UMLAggregationKind._
import org.omg.oti.operations._
import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti._

import scala.reflect.runtime.universe._
import scala.collection.JavaConversions._
import scala.language.postfixOps
import scala.util.Try
import scala.util.Success
import scala.util.Failure

/**
 * Mapping for a kind of binary, directed, composite UML Association to an OMF relationship entity according to IMCE-generated profile stereotypes
 * 
 * The UML association maps to an OMF entity relationship that specializes the OMF entity relationships corresponding to the stereotypes applied.
 * If the UML association does not have any stereotype applied that maps directly or indirectly to an OMF entity relationship,
 * then the mapping is equivalent to the mapping of the same UML association with the IMCE-generated 'base:contains' stereotype applied.
 */
case class R4[Uml <: UML, Omf <: OMF]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  import umlOps._
  import omfOps._

  def binaryCompositeAssociation2RelationshipMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case (
          rule,
          TboxUMLElementPair( Some( tbox ), bcaU: UMLAssociation[Uml] ),
          as, cs, rs, unmappedS ) if ( cs.isEmpty && bcaU.memberEnd.exists( _.aggregation == composite ) && context.getDirectedBinaryAssociationSourceAndTargetMappings(bcaU).isDefined ) =>

          if ( unmappedS.nonEmpty ) {
            val foreign = unmappedS.filter( !context.otherStereotypesApplied.contains( _ ) )
            require( foreign.isEmpty )
          }

          val ( ( sourceTU, sourceOmf ), ( targetTU, targetOmf ) ) = context.getDirectedBinaryAssociationSourceAndTargetMappings(bcaU).get
          
          val hasName = bcaU.name.getOrElse("")
          val hasQualifiedName = bcaU.qualifiedName.getOrElse("")
          
          val ( bcaOmfRelation, bcaOmfGraph ) = context.mapElement2Relationship(
            rule, tbox, bcaU, sourceOmf, targetOmf,
            Iterable(), // @TODO
            isAbstract = bcaU.isAbstract,
            hasName,
            hasQualifiedName )

          val omfRelationshipParents = 
            if ( rs.isEmpty) Set( context.baseContainsR )
            else rs.values
          
          omfRelationshipParents.foreach ( context.addEntityRelationshipSubClassAxiom( rule, tbox, bcaOmfRelation, _ ) )

          Success( ( Nil, Nil ) )
      }

    MappingFunction[Uml, Omf]( "binaryCompositeAssociation2RelationshipMapping", mapping )

  }
}