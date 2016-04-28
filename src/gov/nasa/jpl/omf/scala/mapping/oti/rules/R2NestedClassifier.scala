package gov.nasa.jpl.omf.scala.mapping.oti.rules

import java.lang.System

import gov.nasa.jpl.omf.scala.core._
import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples.TboxUMLNestedClassifier
import gov.nasa.jpl.omf.scala.mapping.oti._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.read.operations._

import scala.{Some, StringContext, Tuple3, Unit}
import scala.collection.immutable._
import scala.language.postfixOps
import scalaz._

case class R2NestedClassifier[Uml <: UML, Omf <: OMF, Provenance]
()
(implicit val umlOps: UMLOps[Uml], omfOps: OMFOps[Omf]) {

  def reportNestedClassifierAsErrorMapping(context: OTI2OMFMappingContext[Uml, Omf, Provenance]) = {

    import gov.nasa.jpl.omf.scala.mapping.oti.TBoxMappingTuples._

    val mapping: OTI2OMFMappingContext[Uml, Omf, Provenance]#RuleFunction = {
      case (rule, TboxUMLNestedClassifier(Some(tbox), nestedClassifier, nestingClass), as, cs, rs, unmappedS) =>

        def record
        (r: (UMLStereotype[Uml], Omf#ModelEntityDefinition))
        : Unit
        = context.recordAppliedStereotype(nestedClassifier)(r._1)

        as.foreach(record)
        cs.foreach(record)
        rs.foreach(record)

        if (unmappedS.nonEmpty) {
          val foreign = unmappedS.filter(!context.otherStereotypesApplied.contains(_))
          if (foreign.nonEmpty) {
            System.out.println(s"*** R2NestedClassifier WARN: ignoring ${foreign.size} unrecognized stereotypes applied to nested classifier: ${nestedClassifier.qualifiedName.get}")
            foreign.foreach { s =>
              System.out.println(s"***  ignoring ${s.qualifiedName.get}")
            }
          }
        }

        val result
        : Set[java.lang.Throwable] \&/ RuleResult[Uml, Omf, Provenance]
        = \&/.This(
          Set(UMLError.illegalElementError[Uml, UMLClassifier[Uml]](
            s"No defined rule for exporting UML Class nested classifier: ${nestedClassifier.qualifiedName.get}",
            Iterable(nestedClassifier)
          ))
        )

        result
    }

    MappingFunction[Uml, Omf, Provenance]("reportNestedClassifierAsErrorMapping", mapping)
  }

}
