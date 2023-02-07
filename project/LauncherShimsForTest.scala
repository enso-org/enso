import sbt.Keys._
import sbt._

object LauncherShimsForTest {

  /** Creates a task that compiles the launcher shims which are used for some of
    * the launcher tests.
    */
  def prepare(): Def.Initialize[Task[Unit]] =
    Def.task {
      val log = state.value.log
      Cargo.run(
        Seq("build", "-p", "launcher-shims"),
        log         = log
      )
    }
}
