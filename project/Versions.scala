/** Versions for some project-wide dependencies.
  *
  * Note [Engine And Launcher Version]
  * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  * Currently both Engine and Launcher versions are tied to each other - each new
  * releases contains the Engine and the Launcher and thus the version number is
  * shared. If the version numbers ever diverge, make sure to update the build
  * scripts at .github/workflows accordingly.
  *
  * Note [Default Editions]
  * ~~~~~~~~~~~~~~~~~~~~~~~
  * Currently, the default edition to use is inferred based on the engine
  * version. Each Enso version has an associated default edition name and the
  * `currentEdition` field specifies the default edition name for the upcoming
  * release.
  *
  * Thus the `library-manager` needs to depend on the `version-output` to get
  * this defaults from the build metadata.
  *
  * In the future we may automate generating this edition number when cutting a
  * release.
  *
  * Note [Stdlib Version]
  * ~~~~~~~~~~~~~~~~~~~~~
  * The `stdlibVersion` variable stores the version at which standard library is
  * stored within the source tree, which is currently set to a constant of
  * `0.0.0-dev`.
  *
  * When distributions are built, the library versions are updated to match the
  * current Enso version.
  */
object Versions {
  val scalacVersion = "2.13.15"
  // source version of the Java language
  val javaVersion = "24"
  // version of the GraalVM JDK
  val graalVersion = "24.0.1"
  // Version used for the Graal/Truffle related Maven packages
  // Keep in sync with GraalVM.version. Do not change the name of this variable,
  // it is used by the Rust build script via regex matching.
  val graalMavenPackagesVersion = "24.2.0"
  val targetJavaVersion         = "17"
  val defaultDevEnsoVersion     = "0.0.0-dev"
  val ensoVersion = sys.env.getOrElse(
    "ENSO_VERSION",
    defaultDevEnsoVersion
  ) // Note [Engine And Launcher Version]
  val currentEdition = sys.env.getOrElse(
    "ENSO_EDITION",
    defaultDevEnsoVersion
  ) // Note [Default Editions]

  // Note [Stdlib Version]
  val stdLibVersion       = defaultDevEnsoVersion
  val targetStdlibVersion = ensoVersion
  val mavenUploadVersion  = "0.2-SNAPSHOT"
}
