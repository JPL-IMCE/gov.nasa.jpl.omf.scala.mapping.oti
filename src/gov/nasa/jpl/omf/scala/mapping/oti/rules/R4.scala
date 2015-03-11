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