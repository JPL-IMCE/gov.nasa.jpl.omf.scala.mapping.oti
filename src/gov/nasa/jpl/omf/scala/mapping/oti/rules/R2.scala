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

case class R2[Uml <: UML, Omf <: OMF]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  import umlOps._
  import omfOps._

  /**
   * Map an OTI UMLNamespace as an OMF aspect according to the mapping of the stereotypes applied.
   */
  def namespace2AspectMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxNestedNamespacePair( Some( tbox ), clsU: UMLClassifier[Uml] ), as, cs, rs, unmappedS ) if ( as.nonEmpty && cs.isEmpty && rs.isEmpty ) =>

          if ( unmappedS.nonEmpty ) {
            System.out.println( unmappedS.map( "<<"+_.qualifiedName.get+">>" ) mkString (
              s"*** ${unmappedS.size} unmapped stereotypes applied:\n- unmapped: ",
              "\nunmapped: ",
              "\n***\n" ) )
          }
          
          val clsOmfAspect = context.element2aspectCtor.applyMapping( rule, tbox, clsU )
          as.foreach { case ( aS, aOmf ) =>
              context.addEntityDefinitionAspectSubClassAxiom( rule, tbox, clsOmfAspect, aOmf )
          }
          
          // for a toplevel package, do we create anything besides the OMF TBox graph?
          // option1: only the OMG TBox graph
          // option2: OMF TBox as the graph for a named individual classified by mappedC (base:Package or its specializations) -- what is this individual in the OMF TBox?
          // option3: OMF TBox as the graph for a concept specializing mappedC (base:Package or its specializations)
    
          // owned UML elements to map in the subsequent content phase
          val pkgContents = clsU.ownedElement.filter( { 
            case _: UMLAssociation[Uml] => true
            case _: UMLNamespace[Uml] => false 
            case _ => true  
          } )
          val moreContents = pkgContents.map( TboxContentPair( tbox, _ ) ) toList;
          
          Success( ( Nil, moreContents ) )
      }

    MappingFunction[Uml, Omf]( "namespace2AspectMapping", context, mapping )

  }
  
  /**
   * Map an OTI UMLNamedElement as an OMF concept according to the mapping of the stereotypes applied.
   */
  def namedElement2ConceptMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxNestedNamespacePair( Some( tbox ), neU: UMLNamedElement[Uml] ), as, cs, rs, unmappedS ) if ( cs.nonEmpty && rs.isEmpty ) =>

          if ( unmappedS.nonEmpty ) {
            System.out.println( unmappedS.map( "<<"+_.qualifiedName.get+">>" ) mkString (
              s"*** ${unmappedS.size} unmapped stereotypes applied:\n- unmapped: ",
              "\nunmapped: ",
              "\n***\n" ) )
          }
          
          val isAbstract = neU match {
            case cls: UMLClassifier[Uml] => cls.isAbstract
            case _ => false
          }
          
          val ( nsOmfConcept, nsOmfGraph ) = context.element2conceptCtor.applyMapping( rule, tbox, neU, isAbstract )
          
          as.foreach { case ( aS, aOmf ) =>
              context.addEntityDefinitionAspectSubClassAxiom( rule, tbox, nsOmfConcept, aOmf )
          }
          
          cs.foreach { case ( cS, cOmf ) =>
              context.addEntityConceptSubClassAxiom( rule, tbox, nsOmfConcept, cOmf )
          }
          
          // owned UML elements to map in the subsequent content phase
          val pkgContents = neU.ownedElement.filter( { 
            case _: UMLAssociation[Uml] => true
            case _: UMLNamespace[Uml] => false 
            case _ => true  
          } )
          val moreContents = pkgContents.map( TboxContentPair( tbox, _ ) ) toList;
          
          Success( ( Nil, moreContents ) )
      }

    MappingFunction[Uml, Omf]( "namedElement2ConceptMapping", context, mapping )

  }
}