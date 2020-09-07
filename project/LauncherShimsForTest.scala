import sbt.Keys._
import sbt._

object LauncherShimsForTest {
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
