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
 * Mapping for a kind of UML Namespace (but not a kind of UML Package) to an OMF aspect or concept entity (but not relationship)
 * 
 * The mapping of a UML Namespace depends on the IMCE-generated profile stereotypes applied (or specializations of).
 * There are 4 kinds of stereotypes:
 * 1) as: Stereotypes that directly or indirectly map into a kind of OMF EntityAspect
 * 2) cs: Stereotypes that directly or indirectly map into a kind of OMF EntityConcept
 * 3) rs: Stereotypes that directly or indirectly map into a kind of OMF EntityRelationship
 * 4) Stereotypes that have no direct or indirect mapping into any kind of OMF EntityDefinition (Aspect, Concept or Relationship)
 * 
 * `namespace2AspectMapping` applies only for (1) -- i.e., some as, no cs, no rs
 * `namedElement2ConceptMapping` applies for (2) -- i.e., some cs, no rs
 */
case class R2[Uml <: UML, Omf <: OMF]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  import umlOps._
  import omfOps._

  /**
   * Map an OTI UMLNamespace as an OMF aspect according to the mapping of the stereotypes applied.
   */
  def namespace2AspectMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxUMLElementPair( Some( tbox ), clsU: UMLClassifier[Uml] ), as, cs, rs, unmappedS ) if ( as.nonEmpty && cs.isEmpty && rs.isEmpty ) =>
          
          if ( unmappedS.nonEmpty ) {
            val foreign = unmappedS.filter( !context.otherStereotypesApplied.contains( _ ) )
            require ( foreign.isEmpty )
          }
          
          val clsOmfAspect = context.mapElement2Aspect( rule, tbox, clsU )
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
          val moreContents = pkgContents.map( TboxUMLElementPair( Some( tbox ), _ ) ) toList;
          
          Success( ( Nil, moreContents ) )
      }

    MappingFunction[Uml, Omf]( "namespace2AspectMapping", mapping )

  }
  
  /**
   * Map an OTI UMLNamedElement as an OMF concept according to the mapping of the stereotypes applied.
   */
  def namedElement2ConceptMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxUMLElementPair( Some( tbox ), neU: UMLNamedElement[Uml] ), as, cs, rs, unmappedS ) if ( cs.nonEmpty && rs.isEmpty ) =>

          if ( unmappedS.nonEmpty ) {
            val foreign = unmappedS.filter( !context.otherStereotypesApplied.contains( _ ) )
            require ( foreign.isEmpty )
          }
          
          val isAbstract = neU match {
            case cls: UMLClassifier[Uml] => cls.isAbstract
            case _ => false
          }
          
          val ( nsOmfConcept, nsOmfGraph ) = context.mapElement2Concept( rule, tbox, neU, isAbstract )
          
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
          val moreContents = pkgContents.map( TboxUMLElementPair( Some( tbox ), _ ) ) toList;
          
          Success( ( Nil, moreContents ) )
      }

    MappingFunction[Uml, Omf]( "namedElement2ConceptMapping", mapping )

  }
}