import sbt.*
import sbt.Keys.*
import sbtassembly.Assembly.{Dependency, JarEntry, Library, Project}
import sbtassembly.MergeStrategy
import sbtassembly.CustomMergeStrategy
import xsbti.compile.IncToolOptionsUtil
import sbt.internal.inc.{CompileOutput, PlainVirtualFile}

/**
 * Collection of utility methods dealing with JPMS modules.
 * The motivation comes from the update of GraalVM to
 * [Truffle unchained](https://medium.com/graalvm/truffle-unchained-13887b77b62c) -
 * we need to add Truffle and Graal related Jars on module-path.
 * We also need to convert our runtime projects to *explicit modules*, and thus,
 * all our other projects to *automatic modules*.
 * @see
 */
object JPMSUtils {
  def filterTruffleAndGraalArtifacts(
    classPath: Def.Classpath
  ): Def.Classpath = {
    val truffleRelatedArtifacts = classPath
      .filter(file => file.data.getPath.contains("graalvm") || file.data.getPath.contains("truffle"))
    truffleRelatedArtifacts
  }

  /**
   * There may be multiple module-info.class files comming from different
   * dependencies. We care only about the one from the `runtime` project.
   * The following merge strategy ensures that all other module-info.class
   * files are discarded from the resulting uber Jar.
   *
   * @param projName Project name for which the module-info.class is retained.
   */
  def removeAllModuleInfoExcept(
    projName: String
  ): MergeStrategy = {
    CustomMergeStrategy(
      strategyName = s"Discard all module-info except for module-info from project $projName",
      notifyIfGTE = 1
    ) { conflictingDeps: Vector[Dependency] =>
      val runtimeModuleInfoOpt = conflictingDeps.collectFirst {
        case project@Project(name, _, _, stream) if name == projName =>
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

  /**
   * Compiles a single `module-info.java` source file with the defined java compiler.
   * Note that sbt is not able to correctly handle `module-info.java` files when
   * compilation order is defined to mixed order.
   * @see https://users.scala-lang.org/t/scala-jdk-11-and-jpms/6102/19
   */
  def compileModuleInfo: Def.Initialize[Task[Unit]] =
    Def.task {
      val moduleInfo = (Compile / javaSource).value / "module-info.java"
      val log = streams.value.log
      val incToolOpts = IncToolOptionsUtil.defaultIncToolOptions()
      val reporter = (Compile / compile / bspReporter).value
      val output = CompileOutput((Compile / classDirectory).value.toPath)
      log.info("Compiling module-info.java with javac")
      val outputAbsPath: String = output
        .getSingleOutputAsPath
        .get()
        .toAbsolutePath
        .toString
      val opts: List[String] =
        (Compile / javacOptions).value.toList ++
          List("-d", outputAbsPath)
      val javaCompiler =
        (Compile / compile / compilers).value.javaTools.javac()
      val succ = javaCompiler.run(
        Array(PlainVirtualFile(moduleInfo.toPath)),
        opts.toArray,
        output,
        incToolOpts,
        reporter,
        log
      )
      if (!succ) {
        log.error(s"Compilation of ${moduleInfo} failed")
      }
    }
      .dependsOn(Compile / compile)
}
