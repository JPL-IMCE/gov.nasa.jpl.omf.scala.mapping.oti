// See: https://github.com/sbt/sbt-site

enablePlugins(PreprocessPlugin)

enablePlugins(SiteScaladocPlugin)

import com.typesafe.sbt.SbtGhPages._

preprocessVars in Preprocess := Map(
  "GIT" -> "github.com",
  "REPO" -> organization.value,
  "CI" -> s"https://travis-ci.org/${organizationName.value}/${name.value}",
  "VER" -> version.value,
  "ORG" -> organizationName.value,
  "SUBJECT" -> organizationName.value,
  "ORG_NAME" -> organizationName.value,
  "DESC" -> description.value,
  "PKG" -> moduleName.value,
  "CONTRIBUTORS" -> {
    val commit = Process("git rev-parse HEAD").lines.head
    val p1 = Process(s"git shortlog -sne --no-merges $commit")
    val p2 = Process(
      Seq("sed",
        "-e",
        """s|^\s*\([0-9][0-9]*\)\s*\(\w.*\w\)\s*<\([a-zA-Z].*\)>.*$|<tr><td align='right'>\1</td><td>\2</td><td>\3</td></tr>|"""))
    val whoswho = p1 #| p2
    whoswho.lines.mkString("\n")
  },
  "VERSION" -> {
    git.gitCurrentTags.value match {
      case Seq(tag) =>
        s"""<a href="https://@GIT@/@ORG@/${moduleName.value}/tree/$tag">$tag</a>"""
      case _ =>
        val v = version.value
        git.gitHeadCommit.value.fold[String](v) { sha =>
          if (git.gitUncommittedChanges.value)
            v
          else
            s"""<a href="https://@GIT@/@ORG@/${moduleName.value}/tree/$sha">$v</a>"""
        }
    }
  }
)

target in preprocess := (target in makeSite).value

ghpages.settings

dependencyDotFile := baseDirectory.value / "target" / "dependencies.dot"

lazy val dependencySvgFile = settingKey[File]("Location of the dependency graph in SVG format")

dependencySvgFile := baseDirectory.value / "target" / "dependencies.svg"

lazy val filter: ScopeFilter = ScopeFilter(inProjects(ThisProject), inConfigurations(Compile))

dumpLicenseReport := {
  val dotFile = dependencyDot.all(filter).value
  val ok = Process(command="dot", arguments=Seq[String]("-Tsvg", dotFile.head.getAbsolutePath, "-o"+dependencySvgFile.value.getAbsolutePath)).!
  require(0 == ok, "dot2svg failed: $ok")
  dumpLicenseReport.value
}

makeSite := {
  val _ = dumpLicenseReport.value
  makeSite.value
}

siteMappings := {
  val _ = dumpLicenseReport.value
  siteMappings.value
}

siteMappings += (licenseReportDir.value / "LicenseReportOfAggregatedSBTPluginsAndLibraries.html") -> "LicenseReportOfAggregatedSBTPluginsAndLibraries.html"
siteMappings += dependencySvgFile.value -> "dependencies.svg"

previewFixedPort := Some(4004)

previewLaunchBrowser := false


releasePublishArtifactsAction := {
  val _ = GhPagesKeys.pushSite.value
  releasePublishArtifactsAction.value
}
