package gov.nasa.jpl.omf.scala.mapping.oti

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.binding._
import org.omg.oti._
import org.omg.oti.api._
import org.omg.oti.operations._
import org.omg.oti.canonicalXMI._
import scala.reflect.runtime.universe._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

case class ResolvedDocumentSet2TBoxGraphMapping[Uml <: UML, Omf <: OMF]()(
  implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

case class ResolvedDocumentTBoxGraph[Uml <: UML, Omf <: OMF](
  document: Document[Uml],
  tbox: Omf#ImmutableModelTerminologyGraph,
  aspects: Map[UMLElement[Uml], Omf#ModelEntityAspect],
  concepts: Map[UMLElement[Uml], Omf#ModelEntityConcept] )(
    implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

case class Document2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF](
  resolved: ResolvedDocumentSet[Uml],
  document2tboxGraphs: Map[Document[Uml], Omf#ImmutableModelTerminologyGraph],
  documents2map: Set[Document[Uml]] )(
    implicit umlOps: UMLOps[Uml], omfOps: OMFOps[Omf], omfStore: Omf#Store )

object ResolvedDocumentSet2TBoxGraphMapping {

  def resolvedDocumentSet2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF]( resolved: ResolvedDocumentSet[Uml] )(
    implicit umlOps: UMLOps[Uml],
    omfOps: OMFOps[Omf],
    omfStore: Omf#Store,
    catalogIRIMapper: CatalogURIMapper ): Try[Document2TBoxGraphCorrespondences[Uml, Omf]] = {

    val sortedDocuments = resolved.ds.topologicalSort( resolved.g ) match {
      case Left( document )         => List( document )
      case Right( sortedDocuments ) => sortedDocuments
    }

    import omfOps._

    val ( document2tboxGraphs, documents2map ) =
      ( ( Map[Document[Uml], Omf#ImmutableModelTerminologyGraph](),
        Set[Document[Uml]]() ) /: sortedDocuments ) {
          case ( ( ( document2tboxMap, documents2map ), document ) ) =>
            catalogIRIMapper.resolveURI( document.uri, catalogIRIMapper.loadResolutionStrategy( _ ) ) match {
              case Failure( t ) =>
                return Failure( t )
              case Success( None ) =>
                System.out.println(s"document2map: ${document.uri}")
                ( document2tboxMap, documents2map + document )
              case Success( Some( uri ) ) =>
                System.out.println(s"document2load: ${document.uri}\nresolved: ${uri}")
                loadTerminologyGraph( makeIRI( document.uri.toString ) ) match {
                  case Failure( t ) => 
                    System.out.println(s"*** document: ${document.uri}\n*** ${t.getClass.getName}: ${t.getMessage}")
                    t.printStackTrace(System.out)
                    //return Failure( t )
                    ( document2tboxMap, documents2map + document )
                  case Success( tbox ) =>
                    System.out.println(s"==> document: ${document.uri}")
                    ( document2tboxMap + ( document -> tbox ), documents2map )
                }
            }
        }

    System.out.println(s"**** Document2TBoxGraphCorrespondences( d2tbox=${document2tboxGraphs.size}, documents2map=${documents2map.size} )")
    Success( Document2TBoxGraphCorrespondences( resolved, document2tboxGraphs, documents2map ) )
  }

  def mapDocument2TBoxGraphCorrespondences[Uml <: UML, Omf <: OMF](
    correspondences: Document2TBoxGraphCorrespondences[Uml, Omf],
    document2BuiltInTBoxGraph: Function1[Document[Uml], Try[Option[Omf#ImmutableModelTerminologyGraph]]] )(
      implicit umlOps: UMLOps[Uml],
      omfOps: OMFOps[Omf],
      omfStore: Omf#Store,
      catalogIRIMapper: CatalogURIMapper ): Try[ResolvedDocumentSet2TBoxGraphMapping[Uml, Omf]] = ???

}
