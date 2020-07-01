import sbt._
import sbt.Keys._
import sbtcrossproject.CrossProject
import sbtcrossproject.CrossPlugin.autoImport._

object RecompileParser {

  /**
    * Ensures that the project is recompiled whenever the project from
    * `syntaxDefinition` is changed. Should be attached to the `compile` task as
    * a dependency.
    */
  def run(syntaxDefinition: CrossProject) =
    Def.taskDyn {
      val parserCompile =
        (syntaxDefinition.jvm / Compile / compileIncremental).value
      if (parserCompile.hasModified) {
        Def.task {
          streams.value.log.info("Parser changed, forcing recompilation.")
          clean.value
        }
      } else Def.task {}
    }
}
