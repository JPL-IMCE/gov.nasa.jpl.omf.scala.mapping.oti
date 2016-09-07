
sbtPlugin := false

name := "gov.nasa.jpl.omf.scala.mapping.oti"

description := "Pattern-based mapping of UML models (queries via the OTI API) to OMF."

moduleName := name.value

organization := "gov.nasa.jpl.imce"

homepage := Some(url(s"https://github.jpl.nasa.gov/imce/${moduleName.value}"))

organizationName := "JPL-IMCE"

organizationHomepage := Some(url(s"https://github.jpl.nasa.gov/imce"))

git.remoteRepo := s"git@github.jpl.nasa.gov:imce/${moduleName.value}"

startYear := Some(2015)

scmInfo := Some(ScmInfo(
  browseUrl = url(s"https://github.jpl.nasa.gov/imce/${moduleName.value}"),
  connection = "scm:"+git.remoteRepo.value))

developers := List(
  Developer(
    id="NicolasRouquette",
    name="Nicolas F. Rouquette",
    email="nicolas.f.rouquette@jpl.nasa.gov",
    url=url("https://github.com/NicolasRouquette")))
