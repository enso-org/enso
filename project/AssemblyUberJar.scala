import sbt.*
import sbtassembly.Assembly.{Project, Library, Dependency, JarEntry}
import sbtassembly.MergeStrategy
import sbtassembly.CustomMergeStrategy

object AssemblyUberJar {
  /**
   * There may be multiple module-info.class files comming from different
   * dependencies. We care only about the one from the `runtime` project.
   * The following merge strategy ensures that all other module-info.class
   * files are discarded from the resulting uber Jar.
   */
  def moduleInfoMergeStrategy: MergeStrategy = {
    CustomMergeStrategy(
      strategyName = "Discard all module-info except for org.enso.runtime",
      notifyIfGTE = 1
    ) { conflictingDeps: Vector[Dependency] =>
      val runtimeModuleInfoOpt = conflictingDeps.collectFirst {
        case project@Project(name, _, _, stream) if name == "runtime" =>
          project
      }
      runtimeModuleInfoOpt match {
        case Some(runtimeModuleInfo) => Right(
          Vector(
            JarEntry(runtimeModuleInfo.target, runtimeModuleInfo.stream)
          )
        )
        case None => Right(Vector())
      }
    }
  }

}
