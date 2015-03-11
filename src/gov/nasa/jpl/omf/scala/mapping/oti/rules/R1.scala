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
 * Mapping for a kind of UML Package (but not a Profile)
 * 
 * The mapping of a UML Package distinguishes 2 kinds of owned elements:
 * - nested packages will be recursively mapped
 * - non-package owned elements will be mapped in the subsequent phase
 * 
 * For the IMCE authorization pattern, a UML Package, as a kind of UML Namespace, should map to an OMF TerminologyGraph.
 * Currently, this rule does not map a UML Package according to the IMCE authorization pattern.
 */
case class R1[Uml <: UML, Omf <: OMF]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  import umlOps._
  import omfOps._

  /**
   * Map an OTI non-profile Package P to OMF
   * If no stereotype is applied to P, treat P as if base:Package had been applied.
   */
  def nonProfilePackageMapping( context: OTI2OMFMappingContext[Uml, Omf] ) = {

    val mapping: OTI2OMFMappingContext[Uml, Omf]#RuleFunction =
      {
        case ( rule, TboxUMLElementPair( Some( tbox ), pkgU: UMLPackage[Uml] ), as, cs, rs, unmappedS ) =>
          require( oclIsTypeOfPackage( pkgU ) )

          val ( mappedS, unmappedS ) = context.partitionAppliedStereotypesByMapping( pkgU )
          if ( unmappedS.nonEmpty ) {
            val foreign = unmappedS.filter( !context.otherStereotypesApplied.contains( _ ) )
            require ( foreign.isEmpty )
          }

          val mappedC =
            if ( mappedS.isEmpty ) Set( context.basePackageC )
            else mappedS.map( context.stereotype2Concept( _ ) )

          val pkgTbox = /* context.ns2tboxCtor( rule, pkgU ) */ tbox // @TODO

          // @TODO
          // for a toplevel package, do we create anything besides the OMF TBox graph?
          // option1: only the OMG TBox graph
          // option2: OMF TBox as the graph for a named individual classified by mappedC (base:Package or its specializations) -- what is this individual in the OMF TBox?
          // option3: OMF TBox as the graph for a concept specializing mappedC (base:Package or its specializations)

          // nested UML namespaces to map in this phase (excludes associations since they must be mapped in the subsequent content phase)
          val pkgNested = pkgU.ownedElement.selectByKindOf( { case ns: UMLNamespace[Uml] => ns } ).filter( {
            case _: UMLAssociation[Uml] => false
            case _                      => true
          } )
          val morePairs = pkgNested.map( TboxUMLElementPair( Some( pkgTbox ), _ ) ) toList;

          // owned UML elements to map in the subsequent content phase
          val pkgContents = pkgU.ownedElement.filter( {
            case _: UMLAssociation[Uml] => true
            case _: UMLNamespace[Uml]   => false
            case _                      => true
          } )
          val moreContents = pkgContents.map( TboxUMLElementPair( Some( pkgTbox ), _ ) ) toList;

          Success( ( morePairs, moreContents ) )
      }

    MappingFunction[Uml, Omf]( "nonProfilePackackageMapping", mapping )

  }
}