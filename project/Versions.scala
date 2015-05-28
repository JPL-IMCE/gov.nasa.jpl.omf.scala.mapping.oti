
import sbt.Process

object Versions {
  val scala = "2.11.6"
  
  // JPL MBEE release
  val jpl_mbee_release_prefix="1800.02-"

  // JPL Ontology Modeling Framework Core
  val jpl_omf_core = jpl_mbee_release_prefix+"e8ef32c064d457923beecfce50a4359a33c1aa22"

  // OTI Trees version
    
  val oti_trees_prefix = "0.14.0"
  val oti_trees_suffix = "770"
  val oti_trees_version = oti_trees_prefix+"-"+oti_trees_suffix

  // JPL MBEE Common Scala Libraries
  val jpl_mbee_common_scala_libraries_revision="60d5d55571aeff7e4fb884ecd8d4e02b0da27ce6"
  val jpl_mbee_core = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision
  val jpl_mbee_other = jpl_mbee_release_prefix+jpl_mbee_common_scala_libraries_revision

}
