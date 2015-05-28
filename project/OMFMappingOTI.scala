import java.io.File

import com.banno.license.Plugin.LicenseKeys._
import com.typesafe.sbt.SbtGit._
import net.virtualvoid.sbt.graph.Plugin.graphSettings
import sbt.Keys._
import sbt._
import xerial.sbt.Pack._

/**
 * sbt \
 * -DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)>
 */
object OMFMappingOTI extends Build {

  lazy val jplSettings = Seq(
    autoScalaLibrary := false,
    scalaVersion := Versions.scala,
    organization := "gov.nasa.jpl.mbee.omf",
    organizationName := "JPL, Caltech",
    organizationHomepage := Some(url("https://mbse.jpl.nasa.gov")),
    publishMavenStyle := false,
    publishTo := {
      Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
        case Some(dir) => Some(Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns))
        case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    },
    resolvers += {
      Option.apply(System.getProperty("JPL_MBEE_LOCAL_REPOSITORY")) match {
        case Some(dir) => Resolver.file("file", new File(dir))(Resolver.ivyStylePatterns)
        case None => sys.error("Set -DJPL_MBEE_LOCAL_REPOSITORY=<dir> where <dir> is a local Ivy repository directory")
      }
    }
  )

  lazy val commonSettings =
    Defaults.coreDefaultSettings ++ Defaults.runnerSettings ++ Defaults.baseTasks ++ graphSettings

  lazy val sourcePublishSettings = Seq(
    // include all test artifacts
    publishArtifact in Test := true
  )

  lazy val owlapi = Project(
    "omf-mapping-oti",
    file(".")).
    settings(versionWithGit: _*).
    settings(showCurrentGitBranch: _*).
    settings(jplSettings: _*).
    settings(commonSettings: _*).
    settings(sourcePublishSettings: _*).
    settings(com.banno.license.Plugin.licenseSettings: _*).
    settings(
      removeExistingHeaderBlock := true,
      scalaSource in Compile := baseDirectory.value / "src",

      libraryDependencies ++= Seq(
        "gov.nasa.jpl.mbee.omf" %% "omf-scala-core" % Versions.jpl_omf_core intransitive() withSources() withJavadoc(),
        "gov.nasa.jpl.mbee.omg.oti" %% "oti-core" % Versions.oti_core_version withJavadoc(),
        "gov.nasa.jpl.mbee.omg.oti" %% "oti-trees" % Versions.oti_trees_version withSources() withJavadoc(),
       "gov.nasa.jpl.mbee" %% "jpl-mbee-common-scala-libraries_core" % Versions.jpl_mbee_core
      )
    )
    
}