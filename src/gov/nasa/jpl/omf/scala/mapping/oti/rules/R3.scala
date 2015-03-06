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

case class R3[Uml <: UML, Omf <: OMF]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  import umlOps._
  import omfOps._

  def dependency2RelationshipMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxNestedNamespacePair( Some( tbox ), depU: UMLDependency[Uml] ), as, cs, rs, unmappedS ) if ( rs.nonEmpty && context.getDependencySourceAndTargetMappings( depU ).isDefined ) =>

          if ( unmappedS.nonEmpty ) {
            System.out.println( unmappedS.map( "<<"+_.qualifiedName.get+">>" ) mkString (
              s"*** ${unmappedS.size} unmapped stereotypes applied:\n- unmapped: ",
              "\nunmapped: ",
              "\n***\n" ) )
          }
          
          val ( ( sourceU, sourceOmf ), ( targetU, targetOmf ) ) = context.getDependencySourceAndTargetMappings( depU ).get
          val r1 = if ( rs.size == 1 ) Some( rs.head._1 ) else None
          val r1Name = if ( r1.isDefined ) r1.get.name.get else ""
          val hasName = sourceU.name.getOrElse(sourceU.id)+"-"+r1Name+"-"+targetU.name.getOrElse(targetU.id)
          val hasQualifiedName = sourceU.qualifiedName.getOrElse(sourceU.id)+"-"+r1Name+"-"+targetU.qualifiedName.getOrElse(targetU.id)
           
          val ( depOmfRelation, depOmfGraph ) = context.element2relationshipCtor.applyMapping(
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

    MappingFunction[Uml, Omf]( "dependency2RelationshipMapping", context, mapping )

  }
}