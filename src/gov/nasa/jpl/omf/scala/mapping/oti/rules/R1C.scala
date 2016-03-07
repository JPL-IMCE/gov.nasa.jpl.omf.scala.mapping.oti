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

case class R1C[Uml <: UML, Omf <: OMF, Provenance]()( implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf] ) {

  def packageContentsMapping
  (context: OTI2OMFMappingContext[Uml, Omf, Provenance],
   pkg2provenance: UMLPackage[Uml] => Provenance) = {

    val mapping
    : OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction
    = {

      case (rule, pair: TBoxPackageContentsConversion[Uml, Omf], as, cs, rs, unmappedS) => {
        val contents =
          pair.e.packagedElement.filter {
          case _: UMLPackage[Uml] =>
            false
          case _: UMLPackageImport[Uml] =>
            false
          case _: UMLElementImport[Uml] =>
            false
          case _: UMLPackageMerge[Uml] =>
            false
          case _ =>
            true
        }

        \&/.That(RuleResult[Uml, Omf, Provenance](
          rule,
          finalResults = Vector(),
          internalResults = Vector(),
          externalResults = contents.map(TboxUMLElementTuple(pair.pkgDocumentTbox.some, _)).toVector))
      }
    }

    MappingFunction[Uml, Omf, Provenance]("R1B (packageContentsMapping)", mapping)
  }


}
