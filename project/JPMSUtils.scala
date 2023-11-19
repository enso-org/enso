import sbt.*
import sbt.Keys.*
import sbtassembly.Assembly.{Dependency, JarEntry, Library, Project}
import sbtassembly.MergeStrategy

import java.io.{File, FilenameFilter}
import sbtassembly.CustomMergeStrategy
import xsbti.compile.IncToolOptionsUtil
import sbt.internal.inc.{CompileOutput, PlainVirtualFile}

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{
  FileVisitResult,
  FileVisitor,
  Files,
  Path,
  SimpleFileVisitor
}

/** Collection of utility methods dealing with JPMS modules.
  * The motivation comes from the update of GraalVM to
  * [Truffle unchained](https://medium.com/graalvm/truffle-unchained-13887b77b62c) -
  * we need to add Truffle and Graal related Jars on module-path.
  * We also need to convert our runtime projects to *explicit modules*, and thus,
  * all our other projects to *automatic modules*.
  * @see
  */
object JPMSUtils {
  val slf4jVersion          = "2.0.9"
  val logbackClassicVersion = "1.3.7"

  /** The list of modules that are included in the `component` directory in engine distribution.
    * When invoking the `java` command, these modules need to be put on the module-path.
    */
  val componentModules: Seq[ModuleID] =
    GraalVM.modules ++ GraalVM.langsPkgs ++ Seq(
      "org.slf4j"      % "slf4j-api"       % slf4jVersion,
      "ch.qos.logback" % "logback-classic" % logbackClassicVersion,
      "ch.qos.logback" % "logback-core"    % logbackClassicVersion
    )

  /** Filters modules by their IDs from the given classpath.
    * @param cp The classpath to filter
    * @param modules These modules are looked for in the class path
    * @param shouldContainAll If true, the method will throw an exception if not all modules were found
    *                         in the classpath.
    * @return The classpath with only the provided modules searched by their IDs.
    */
  def filterModulesFromClasspath(
    cp: Def.Classpath,
    modules: Seq[ModuleID],
    log: sbt.util.Logger,
    shouldContainAll: Boolean = false
  ): Def.Classpath = {
    def shouldFilterModule(module: ModuleID): Boolean = {
      modules.exists(m =>
        m.organization == module.organization &&
        m.name == module.name &&
        m.revision == module.revision
      )
    }
    val ret = cp.filter(dep => {
      val moduleID = dep.metadata.get(AttributeKey[ModuleID]("moduleID")).get
      shouldFilterModule(moduleID)
    })
    if (shouldContainAll) {
      if (ret.size < modules.size) {
        log.error("Not all modules from classpath were found")
        log.error(s"Returned (${ret.size}): $ret")
        log.error(s"Expected: (${modules.size}): $modules")
      }
    }
    ret
  }

  def filterTruffleAndGraalArtifacts(
    classPath: Def.Classpath
  ): Def.Classpath = {
    val truffleRelatedArtifacts = classPath
      .filter(file =>
        file.data.getPath.contains("graalvm") || file.data.getPath.contains(
          "truffle"
        )
      )
    truffleRelatedArtifacts
  }

  /** There may be multiple module-info.class files comming from different
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
      strategyName =
        s"Discard all module-info except for module-info from project $projName",
      notifyIfGTE = 1
    ) { conflictingDeps: Vector[Dependency] =>
      val runtimeModuleInfoOpt = conflictingDeps.collectFirst {
        case project @ Project(name, _, _, stream) if name == projName =>
          project
      }
      runtimeModuleInfoOpt match {
        case Some(runtimeModuleInfo) =>
          Right(
            Vector(
              JarEntry(runtimeModuleInfo.target, runtimeModuleInfo.stream)
            )
          )
        case None => Right(Vector())
      }
    }
  }

  /** Compiles a single `module-info.java` source file with the default java compiler (
    * the one that is defined for the project). Before the module-info is compiled, all the
    * class files from `scopeFilter` are copied into the `target` directory of the current project.
    * This is because we want the `module-info.java` to be an *Uber module-info* that is defined for
    * multiple sbt projects, like `runtime` and `runtime-with-instruments`, so before the `module-info.java`
    * is passed to the compiler, we need to copy all the classes from the sbt projects into a single directory
    * so that the compiler has an illusion that all these projects are in fact a single project.
    *
    * Note that sbt is not able to correctly handle `module-info.java` files when
    * compilation order is defined to mixed order.
    *
    * @param copyDepsFilter The filter of scopes of the projects from which the class files are first
    *                    copied into the `target` directory before `module-info.java` is compiled.
    * @param modulePath IDs of dependencies that should be put on the module path. The modules
    *                   put into `modulePath` are filtered away from class-path, so that module-path
    *                   and class-path passed to the `javac` are exclusive.
    *
    * @see https://users.scala-lang.org/t/scala-jdk-11-and-jpms/6102/19
    */
  def compileModuleInfo(
    copyDepsFilter: ScopeFilter,
    modulePath: Seq[ModuleID] = Seq()
  ): Def.Initialize[Task[Unit]] =
    Def
      .task {
        val moduleInfo  = (Compile / javaSource).value / "module-info.java"
        val log         = streams.value.log
        val incToolOpts = IncToolOptionsUtil.defaultIncToolOptions()
        val reporter    = (Compile / compile / bspReporter).value
        val output      = CompileOutput((Compile / classDirectory).value.toPath)
        // Target directory where all the classes from all the dependencies will be
        // copied to.
        val outputPath: Path = output.getSingleOutputAsPath
          .get()

        /** Copy classes into the target directory from all the dependencies */
        log.debug(s"Copying classes to $output")
        val sourceProducts = products.all(copyDepsFilter).value.flatten

        if (!(outputPath.toFile.exists())) {
          Files.createDirectory(outputPath)
        }

        val outputLangProvider =
          outputPath / "META-INF" / "services" / "com.oracle.truffle.api.provider.TruffleLanguageProvider"
        sourceProducts.foreach { sourceProduct =>
          log.debug(s"Copying ${sourceProduct} to ${output}")
          val sourceLangProvider =
            sourceProduct / "META-INF" / "services" / "com.oracle.truffle.api.provider.TruffleLanguageProvider"
          if (
            outputLangProvider.toFile.exists() && sourceLangProvider.exists()
          ) {
            log.debug(
              s"Merging ${sourceLangProvider} into ${outputLangProvider}"
            )
            val sourceLines = IO.readLines(sourceLangProvider)
            val destLines   = IO.readLines(outputLangProvider.toFile)
            val outLines    = (sourceLines ++ destLines).distinct
            IO.writeLines(outputLangProvider.toFile, outLines)
          }
          // Copy the rest of the directory - don't override META-INF.
          IO.copyDirectory(
            sourceProduct,
            outputPath.toFile,
            CopyOptions(
              overwrite            = false,
              preserveLastModified = true,
              preserveExecutable   = true
            )
          )
        }

        log.info("Compiling module-info.java with javac")
        val baseJavacOpts = (Compile / javacOptions).value
        val fullCp        = (Compile / fullClasspath).value
        val (mp, cp) = fullCp.partition(file => {
          val moduleID =
            file.metadata.get(AttributeKey[ModuleID]("moduleID")).get
          modulePath.exists(mod => {
            mod.organization == moduleID.organization &&
            mod.name == moduleID.name &&
            mod.revision == moduleID.revision
          })
        })

        val allOpts = baseJavacOpts ++ Seq(
          "--class-path",
          cp.map(_.data.getAbsolutePath).mkString(File.pathSeparator),
          "--module-path",
          mp.map(_.data.getAbsolutePath).mkString(File.pathSeparator),
          "-d",
          outputPath.toAbsolutePath().toString()
        )
        val javaCompiler =
          (Compile / compile / compilers).value.javaTools.javac()
        val succ = javaCompiler.run(
          Array(PlainVirtualFile(moduleInfo.toPath)),
          allOpts.toArray,
          output,
          incToolOpts,
          reporter,
          log
        )
        if (!succ) {
          sys.error(s"Compilation of ${moduleInfo} failed")
        }
      }
      .dependsOn(Compile / compile)

}
