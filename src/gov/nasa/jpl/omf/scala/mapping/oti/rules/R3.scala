package gov.nasa.jpl.omf.scala.mapping.oti.rules

import org.omg.oti._
import org.omg.oti.api._
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
 * Mapping for a kind of UML Dependency to an OMF relationship entity according to IMCE-generated profile stereotypes
 * 
 * There must at least 1 stereotype applied to the dependency that maps directly or indirectly to a kind of OMF relationship entity.
 * The UML dependency maps to an OMF entity relationship that specializes the OMF entity relationships corresponding to the stereotypes applied.
 */
case class R3[Uml <: UML, Omf <: OMF]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  import umlOps._
  import omfOps._

  def dependency2RelationshipMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxUMLElementPair( Some( tbox ), depU: UMLDependency[Uml] ), as, cs, rs, unmappedS ) if ( rs.nonEmpty && context.getDependencySourceAndTargetMappings( depU ).isDefined ) =>

          if ( unmappedS.nonEmpty ) {
            val foreign = unmappedS.filter( !context.otherStereotypesApplied.contains( _ ) )
            require ( foreign.isEmpty )
          }
          
          val ( ( sourceU, sourceOmf ), ( targetU, targetOmf ) ) = context.getDependencySourceAndTargetMappings( depU ).get
          val r1 = if ( rs.size == 1 ) Some( rs.head._1 ) else None
          val r1Name = if ( r1.isDefined ) r1.get.name.get else ""
          val hasName = sourceU.name.getOrElse(sourceU.id)+"-"+r1Name+"-"+targetU.name.getOrElse(targetU.id)
          val hasQualifiedName = sourceU.qualifiedName.getOrElse(sourceU.id)+"-"+r1Name+"-"+targetU.qualifiedName.getOrElse(targetU.id)
           
          val ( depOmfRelation, depOmfGraph ) = context.mapElement2Relationship(
              rule, tbox, depU, sourceOmf, targetOmf, 
              Iterable(), // @TODO
              isAbstract=false,
              hasName, 
              hasQualifiedName)
              
          rs.foreach { case ( relUml, relOmf ) =>
            
             context.addEntityRelationshipSubClassAxiom(rule, tbox, depOmfRelation, relOmf )
             
          }
          
          Success( ( Nil, Nil ) )
      }

    MappingFunction[Uml, Omf]( "dependency2RelationshipMapping", mapping )

  }
}