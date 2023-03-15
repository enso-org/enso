import sbt._
import sbt.Keys._

object RecompileParser {

  /** Ensures that the project is recompiled whenever the project from
    * `syntaxDefinition` is changed. Should be attached to the `compile` task as
    * a dependency.
    */
  def run(syntaxDefinition: Project) =
    Def.taskDyn {
      val parserCompile =
        (syntaxDefinition / Compile / compileIncremental).value
      if (parserCompile.hasModified) {
        Def.task {
          streams.value.log.info("Parser changed, forcing recompilation.")
          clean.value
        }
      } else Def.task {}
    }
}
