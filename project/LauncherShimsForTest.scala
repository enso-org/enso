import sbt.Keys._
import sbt._

object LauncherShimsForTest {

  /**
    * Creates a task that compiles the launcher shims which are used for some of
    * the launcher tests.
    *
    * @param rustcVersion Rust version that should be used
    */
  def prepare(rustcVersion: String): Def.Initialize[Task[Unit]] =
    Def.task {
      val log = state.value.log
      val launcherLocation = NativeImage
        .artifactFile("enso")
        .toPath
        .toAbsolutePath
        .normalize
        .toString
      Cargo.run(
        "build -p launcher-shims",
        rustVersion = rustcVersion,
        log         = log,
        extraEnv    = Seq("ENSO_LAUNCHER_LOCATION" -> launcherLocation)
      )
    }
}
