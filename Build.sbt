import gov.nasa.jpl.mbee.sbt._
import sbt.Keys._
import sbt._

lazy val core = Project("omf-mapping-oti", file("."))
  .settings(GitVersioning.buildSettings) // in principle, unnecessary; in practice: doesn't work without this
  .enablePlugins(MBEEGitPlugin)
  .settings(MBEEPlugin.mbeeDynamicScriptsProjectResourceSettings(Some("gov.nasa.jpl.omf.scala.mapping.oti")))
  .settings(
    MBEEKeys.mbeeLicenseYearOrRange := "2014-2015",
    MBEEKeys.mbeeOrganizationInfo := MBEEPlugin.MBEEOrganizations.imce,
    MBEEKeys.targetJDK := MBEEKeys.jdk17.value,
    // include all test artifacts
    publishArtifact in Test := true,

    scalaSource in Compile := baseDirectory.value / "src",
    classDirectory in Compile := baseDirectory.value / "bin",

    // TODO: Jenkins CI: This should be unnecessary since the repo is in the library dependency POM!!!
    resolvers += new MavenRepository("bintray-pchiusano-scalaz-stream", "http://dl.bintray.com/pchiusano/maven"),

    libraryDependencies ++= Seq(
      MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion(
        "jpl-mbee-common-scala-libraries_core",
        MBEEKeys.mbeeReleaseVersionPrefix.value,
        Versions.jpl_mbee_common_scala_libraries_revision),
      MBEEPlugin.MBEEOrganizations.imce.mbeeZipArtifactVersion(
        "jpl-mbee-common-scala-libraries_other",
        MBEEKeys.mbeeReleaseVersionPrefix.value,
        Versions.jpl_mbee_common_scala_libraries_revision),
      MBEEPlugin.MBEEOrganizations.imce.mbeeArtifactVersion(
        "omf-scala-core",
        MBEEKeys.mbeeReleaseVersionPrefix.value,
        Versions.jpl_omf_core_revision) % "compile" withSources() withJavadoc(),
      MBEEPlugin.MBEEOrganizations.imce.mbeeArtifactVersion(
        "omf-scala-core",
        MBEEKeys.mbeeReleaseVersionPrefix.value,
        Versions.jpl_omf_core_revision) % "test" classifier "tests"
        artifacts(
          Artifact.classified("omf-scala-core", "tests-sources"),
          Artifact.classified("omf-scala-core", "tests-javadoc")),
      MBEEPlugin.MBEEOrganizations.oti.mbeeArtifactVersion(
        "oti-canonical-xmi",
        Versions.oti_canonical_xmi_prefix,
        Versions.oti_canonical_xmi_suffix) % "compile" withSources() withJavadoc(),
      MBEEPlugin.MBEEOrganizations.oti.mbeeArtifactVersion(
        "oti-trees",
        Versions.oti_trees_prefix,
        Versions.oti_trees_suffix) % "compile" withSources() withJavadoc()
  )
)