
import sbt.Keys._
import sbt._
import scala.language.postfixOps

import gov.nasa.jpl.imce.sbt._
import gov.nasa.jpl.imce.sbt.ProjectHelper._

import java.io.File

updateOptions := updateOptions.value.withCachedResolution(true)

resolvers := {
  val previous = resolvers.value
  if (git.gitUncommittedChanges.value)
    Seq[Resolver](Resolver.mavenLocal) ++ previous
  else
    previous
}

import scala.io.Source
import scala.util.control.Exception._

lazy val core = 
  Project("omf-scala-mapping-oti", file("."))
  .enablePlugins(IMCEGitPlugin)
  .enablePlugins(IMCEReleasePlugin)
  .settings(dynamicScriptsResourceSettings("gov.nasa.jpl.omf.scala.mapping.oti"))
  .settings(IMCEPlugin.strictScalacFatalWarningsSettings)
  .settings(IMCEReleasePlugin.packageReleaseProcessSettings)
  .settings(
    IMCEKeys.licenseYearOrRange := "2015",
    IMCEKeys.organizationInfo := IMCEPlugin.Organizations.omf,

    buildInfoPackage := "gov.nasa.jpl.omf.scala.mapping.oti",
    buildInfoKeys ++= Seq[BuildInfoKey](BuildInfoKey.action("buildDateUTC") { buildUTCDate.value }),

    projectID := {
      val previous = projectID.value
      previous.extra(
        "build.date.utc" -> buildUTCDate.value,
        "artifact.kind" -> "generic.library")
    },

    IMCEKeys.targetJDK := IMCEKeys.jdk18.value,
    git.baseVersion := Versions.version,

    scalacOptions in (Compile,doc) ++= Seq("-diagrams"),

    resolvers += Resolver.bintrayRepo("jpl-imce", "gov.nasa.jpl.imce"),
    resolvers += Resolver.bintrayRepo("tiwg", "org.omg.tiwg"),

    extractArchives := {}
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "oti-uml-composite_structure_tree_analysis",
    "org.omg.oti.uml.composite_structure_tree_analysis",
    Seq(
      //      //  extra("artifact.kind" -> "generic.library")
      "org.omg.tiwg" %% "org.omg.oti.uml.composite_structure_tree_analysis"
        % Versions_oti_uml_composite_structure_tree_analysis.version
        % "compile" withSources() withJavadoc() artifacts
        Artifact("org.omg.oti.uml.composite_structure_tree_analysis", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "oti-uml-canonical_xmi-serialization",
    "org.omg.oti.uml.canonical_xmi.serialization",
    Seq(
      "org.omg.tiwg" %% "org.omg.oti.uml.canonical_xmi.serialization"
        % Versions_oti_uml_canonical_xmi_serialization.version
        % "compile" withSources() withJavadoc() artifacts
        Artifact("org.omg.oti.uml.canonical_xmi.serialization", "zip", "zip", Some("resource"), Seq(), None, Map())
    )
  )
  .dependsOnSourceProjectOrLibraryArtifacts(
    "omf-scala-core",
    "gov.nasa.jpl.omf.scala.core",
    Seq(
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.core"
        % Versions_omf_scala_core.version
        % "compile" artifacts
        Artifact("gov.nasa.jpl.omf.scala.core", "zip", "zip", Some("resource"), Seq(), None, Map()),
      "gov.nasa.jpl.imce" %% "gov.nasa.jpl.omf.scala.core"
        % Versions_omf_scala_core.version
        % "test->compile;compile->compile" artifacts(
        Artifact("gov.nasa.jpl.omf.scala.core"),
        Artifact("gov.nasa.jpl.omf.scala.core", "tests"))
    )
  )

def dynamicScriptsResourceSettings(projectName: String): Seq[Setting[_]] = {

  import com.typesafe.sbt.packager.universal.UniversalPlugin.autoImport._

  def addIfExists(f: File, name: String): Seq[(File, String)] =
    if (!f.exists) Seq()
    else Seq((f, name))

  val QUALIFIED_NAME = "^[a-zA-Z][\\w_]*(\\.[a-zA-Z][\\w_]*)*$".r

  Seq(
    // the '*-resource.zip' archive will start from: 'dynamicScripts'
    com.typesafe.sbt.packager.Keys.topLevelDirectory in Universal := None,

    // name the '*-resource.zip' in the same way as other artifacts
    com.typesafe.sbt.packager.Keys.packageName in Universal :=
      normalizedName.value + "_" + scalaBinaryVersion.value + "-" + version.value + "-resource",

    // contents of the '*-resource.zip' to be produced by 'universal:packageBin'
    mappings in Universal ++= {
      val dir = baseDirectory.value
      val bin = (packageBin in Compile).value
      val src = (packageSrc in Compile).value
      val doc = (packageDoc in Compile).value
      val binT = (packageBin in Test).value
      val srcT = (packageSrc in Test).value
      val docT = (packageDoc in Test).value

      (dir * ".classpath").pair(rebase(dir, projectName)) ++
        (dir * "*.md").pair(rebase(dir, projectName)) ++
        (dir / "resources" ***).pair(rebase(dir, projectName)) ++
        addIfExists(bin, projectName + "/lib/" + bin.name) ++
        addIfExists(binT, projectName + "/lib/" + binT.name) ++
        addIfExists(src, projectName + "/lib.sources/" + src.name) ++
        addIfExists(srcT, projectName + "/lib.sources/" + srcT.name) ++
        addIfExists(doc, projectName + "/lib.javadoc/" + doc.name) ++
        addIfExists(docT, projectName + "/lib.javadoc/" + docT.name)
    },

    artifacts += {
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map())
    },
    packagedArtifacts += {
      val p = (packageBin in Universal).value
      val n = (name in Universal).value
      Artifact(n, "zip", "zip", Some("resource"), Seq(), None, Map()) -> p
    }
  )
}