import JPMSPlugin.autoImport.{javaModuleName, modulePath}
import sbt._
import sbt.Keys._
import sbt.internal.inc.{CompileOutput, PlainVirtualFile}
import sbt.util.CacheStore
import xsbti.compile.IncToolOptionsUtil

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import scala.collection.mutable

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

  /** Filters modules by their IDs from the given classpath.
    *
    * @param cp               The classpath to filter
    * @param modules          These modules are looked for in the class path, can be duplicated
    * @param projName Name of the current sbt project for debugging
    * @param shouldContainAll If true, the method will throw an exception if not all modules were found
    *                         in the classpath
    * @param scalaBinaryVersion Scala version used in all dependencies
    * @return The classpath with only the provided modules searched by their IDs
    */
  def filterModulesFromClasspath(
    cp: Def.Classpath,
    modules: Seq[ModuleID],
    log: sbt.util.Logger,
    projName: String,
    scalaBinaryVersion: String,
    shouldContainAll: Boolean = false
  ): Def.Classpath = {
    val distinctModules = modules.distinct

    val ret = cp.filter(dep => {
      val moduleID = dep.metadata.get(AttributeKey[ModuleID]("moduleID")).get
      shouldFilterModule(distinctModules, scalaBinaryVersion)(moduleID)
    })

    if (shouldContainAll) {
      if (ret.size < distinctModules.size) {
        log.error(
          s"[JPMSUtils/$projName] Not all modules from classpath were found"
        )
        log.error(
          s"[JPMSUtils/$projName] Ensure libraryDependencies and moduleDependencies are correct"
        )
        log.error(s"[JPMSUtils/$projName] Returned (${ret.size}): $ret")
        log.error(
          s"[JPMSUtils/$projName] Expected: (${distinctModules.size}): $distinctModules"
        )
      }
    }
    ret
  }

  /** Filters all the requested modules from the given [[UpdateReport]].
    *
    * @param updateReport       The update report to filter. This is the result of `update.value`.
    * @param modules            The modules to filter from the update report. Can be duplicated.
    * @param log                The logger to use for logging.
    * @param projName           Name of the current sbt project for debugging.
    * @param scalaBinaryVersion Scala version used in all dependencies
    * @param shouldContainAll   If true, the method will log an error if not all modules were found.
    * @return The list of files (Jar archives, directories, etc.) that were found in the update report.
    */
  def filterModulesFromUpdate(
    updateReport: UpdateReport,
    modules: Seq[ModuleID],
    log: sbt.util.Logger,
    projName: String,
    scalaBinaryVersion: String,
    shouldContainAll: Boolean = false
  ): Seq[File] = {
    val distinctModules = modules.distinct

    val foundFiles = updateReport.select(
      module = shouldFilterModule(distinctModules, scalaBinaryVersion)
    )
    if (shouldContainAll) {
      if (foundFiles.size < distinctModules.size) {
        log.error(
          s"[JPMSUtils/$projName] Not all modules from update were found"
        )
        log.error(
          s"[JPMSUtils/$projName] Ensure libraryDependencies and moduleDependencies are correct"
        )
        log.error(
          s"[JPMSUtils/$projName] Returned (${foundFiles.size}): $foundFiles"
        )
        log.error(
          s"[JPMSUtils/$projName] Expected: (${distinctModules.size}): $distinctModules"
        )
      }
    }
    foundFiles
  }

  def shouldFilterModule(
    distinctModules: Seq[ModuleID],
    scalaBinaryVersion: String
  )(module: ModuleID): Boolean = {
    distinctModules.exists(m =>
      m.organization == module.organization &&
      (m.name == module.name || m.crossVersion.isInstanceOf[
        sbt.librarymanagement.Binary
      ] && s"${m.name}_$scalaBinaryVersion" == module.name) &&
      m.revision == module.revision
    )
  }

  def filterArtifacts(
    classPath: Def.Classpath,
    predicates: String*
  ): Def.Classpath = {
    val truffleRelatedArtifacts =
      classPath.filter(file =>
        predicates.exists(p => file.data.getPath.contains(p))
      )
    truffleRelatedArtifacts
  }

  lazy val extraMp = taskKey[Def.Classpath](
    "Additional internal projects to put on the module path"
  )

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
    * Compilation of `module-info.java` is skipped iff none of all the classes from all the dependencies
    * changed and if the `module-info.java` itself have not changed.
    *
    * To put internal modules (any sbt project defined in `build.sbt`) on module path, use `extraMp` setting.
    *
    * Note that this task will only work if it is defined inside `Compile` scope. In other words, it won't work
    * e.g. in `Test` scope.
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
        // Class directories of all the dependencies.
        val sourceProducts =
          productDirectories.all(copyDepsFilter).value.flatten

        val extraModuleDirs = (Compile / extraMp).value.map(_.data)
        val moduleName      = javaModuleName.value
        val cacheStore      = streams.value.cacheStoreFactory
        val repoRootDir     = (LocalProject("enso") / baseDirectory).value
        var someDepChanged  = false
        sourceProducts.foreach(sourceProduct => {
          if (!sourceProduct.exists()) {
            log.error(s"Source product ${sourceProduct} does not exist")
            log.error(
              "This means that the Compile/compile task was probably not run in " +
              "the corresponding project"
            )
            log.error("Run Compile/compile before this task")
          }
          val relPath    = repoRootDir.toPath.relativize(sourceProduct.toPath)
          val cache      = cacheStore.make("cache-" + relPath.toString)
          val depChanged = copyClasses(sourceProduct, output, cache, log)
          if (depChanged) {
            someDepChanged = true
          }
        })

        val baseJavacOpts = (Compile / javacOptions).value
        val fullCp        = (Compile / fullClasspath).value
        val javaCompiler =
          (Compile / compile / compilers).value.javaTools.javac()

        // Skip module-info.java compilation if the source have not changed
        // Force the compilation if some class file from one of the dependencies changed,
        // just to be sure that we don't cause any weird compilation errors.
        val moduleInfoCache = cacheStore.make("cache-module-info-" + moduleName)
        Tracked.diffInputs(moduleInfoCache, FileInfo.lastModified)(
          Set(moduleInfo)
        ) { changeReport =>
          if (
            someDepChanged || changeReport.modified.nonEmpty || changeReport.added.nonEmpty
          ) {
            log.info(s"Compiling $moduleInfo with javac")
            val (mp, cp) = fullCp.partition(file => {
              val moduleID =
                file.metadata.get(AttributeKey[ModuleID]("moduleID")).get
              modulePath.exists(mod => {
                mod.organization == moduleID.organization &&
                mod.name == moduleID.name &&
                mod.revision == moduleID.revision
              })
            })
            val allDirsOnMp = mp.map(_.data) ++ extraModuleDirs

            val allOpts = baseJavacOpts ++ Seq(
              "--class-path",
              cp.map(_.data.getAbsolutePath).mkString(File.pathSeparator),
              "--module-path",
              allDirsOnMp.map(_.getAbsolutePath).mkString(File.pathSeparator),
              "-d",
              outputPath.toAbsolutePath.toString
            )
            log.debug(s"javac options: $allOpts")

            val succ = javaCompiler.run(
              Array(PlainVirtualFile(moduleInfo.toPath)),
              allOpts.toArray,
              output,
              incToolOpts,
              reporter,
              log
            )
            if (!succ) {
              val msg = s"Compilation of ${moduleInfo} failed"
              log.error(s"javac options: $allOpts")
              log.error(msg)
              throw new IllegalStateException(msg)
            }
          }
        }
      }

  /** Compiles `module-info.java` in the current project. The module path is
    * gathered from [[JPMSPlugin.autoImport.modulePath]] settings.
    * @see [[compileModuleInfo]].
    */
  def compileModuleInfo(): Def.Initialize[Task[Unit]] = Def.task {
    val moduleInfo       = (Compile / javaSource).value / "module-info.java"
    val projName         = moduleName.value
    val log              = streams.value.log
    val incToolOpts      = IncToolOptionsUtil.defaultIncToolOptions()
    val reporter         = (Compile / compile / bspReporter).value
    val output           = CompileOutput((Compile / classDirectory).value.toPath)
    val outputPath: Path = output.getSingleOutputAsPath.get()
    val mp               = (Compile / modulePath).value
    val baseJavacOpts    = (Compile / javacOptions).value
    val cp               = (Compile / fullClasspath).value
    val javaCompiler =
      (Compile / compile / compilers).value.javaTools.javac()
    val cache =
      streams.value.cacheStoreFactory.make("cache-module-info-" + projName)

    Tracked.diffInputs(cache, FileInfo.lastModified)(
      Set(moduleInfo)
    ) { changeReport =>
      if (changeReport.modified.nonEmpty || changeReport.added.nonEmpty) {
        log.info(s"Compiling $moduleInfo with javac")
        val allOpts = baseJavacOpts ++ Seq(
          "--class-path",
          cp.map(_.data.getAbsolutePath).mkString(File.pathSeparator),
          "--module-path",
          mp.map(_.getAbsolutePath).mkString(File.pathSeparator),
          "-d",
          outputPath.toAbsolutePath.toString
        )
        log.debug(s"javac options: $allOpts")
        val succ = javaCompiler.run(
          Array(PlainVirtualFile(moduleInfo.toPath)),
          allOpts.toArray,
          output,
          incToolOpts,
          reporter,
          log
        )
        if (!succ) {
          val msg = s"Compilation of ${moduleInfo} failed"
          log.error(s"javac options: $allOpts")
          log.error(msg)
          throw new IllegalStateException(msg)
        }
      }
    }
  }

  /** Copies all classes from all the dependencies `classes` directories into the target directory.
    * @param sourceClassesDir Directory from where the classes will be copied.
    * @param output Target directory where all the classes from all the dependencies
    *               will be copied to.
    * @param log
    * @return True iff some of the dependencies changed, i.e., if there is a modified class file, or
    *         some class file was added, or removed
    */
  private def copyClasses(
    sourceClassesDir: File,
    output: xsbti.compile.Output,
    cache: CacheStore,
    log: Logger
  ): Boolean = {
    require(sourceClassesDir.isDirectory)
    val outputPath: Path = output.getSingleOutputAsPath.get()
    val outputDir        = outputPath.toFile
    val filesToCopy      = mutable.HashSet.empty[File]
    val fileVisitor = new SimpleFileVisitor[Path] {
      override def visitFile(
        path: Path,
        attrs: BasicFileAttributes
      ): FileVisitResult = {
        if (!path.toFile.isDirectory) {
          filesToCopy.add(path.toFile)
        }
        FileVisitResult.CONTINUE
      }

      override def preVisitDirectory(
        dir: Path,
        attrs: BasicFileAttributes
      ): FileVisitResult = {
        // We do not care about files in META-INF directory. Everything should be described
        // in the `module-info.java`.
        if (dir.getFileName.toString == "META-INF") {
          FileVisitResult.SKIP_SUBTREE
        } else {
          FileVisitResult.CONTINUE
        }
      }
    }
    Files.walkFileTree(sourceClassesDir.toPath, fileVisitor)
    if (!outputDir.exists()) {
      IO.createDirectory(outputDir)
    }
    var someDependencyChanged = false
    Tracked.diffInputs(cache, FileInfo.lastModified)(filesToCopy.toSet) {
      changeReport =>
        for (f <- changeReport.removed) {
          val relPath = sourceClassesDir.toPath.relativize(f.toPath)
          val dest    = outputDir.toPath.resolve(relPath)
          IO.delete(dest.toFile)
          someDependencyChanged = true
        }
        for (f <- changeReport.modified -- changeReport.removed) {
          val relPath = sourceClassesDir.toPath.relativize(f.toPath)
          val dest    = outputDir.toPath.resolve(relPath)
          IO.copyFile(f, dest.toFile)
          someDependencyChanged = true
        }
        for (f <- changeReport.unmodified) {
          val relPath = sourceClassesDir.toPath.relativize(f.toPath)
          val dest    = outputDir.toPath.resolve(relPath)
          if (!dest.toFile.exists()) {
            IO.copyFile(f, dest.toFile)
            someDependencyChanged = true
          }
        }
    }
    someDependencyChanged
  }
}
