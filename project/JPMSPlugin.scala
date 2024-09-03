import sbt.*
import sbt.Keys.*
import sbt.internal.util.ManagedLogger

import java.io.File
import java.net.{URI, URL}
import java.util.jar.JarFile
import scala.collection.mutable

/** An automatic plugin that handles everything related to JPMS modules. One needs to explicitly
  * enable this plugin in a project with `.enablePlugins(JPMSPlugin)`. The keys and tasks provided by this plugin
  * corresponds to the module-related options of `javac` and `java` commands.
  *
  * This plugin injects all the module-specific options to `javaOptions`, based on
  * the settings of this plugin.
  *
  * If this plugin is enabled, and no settings/tasks from this plugin are used, then the plugin will
  * not inject anything into `javaOptions` or `javacOptions`.
 *
 * - `compileOrder` has to be specified before `libraryDependencies`
  */
object JPMSPlugin extends AutoPlugin {
  object autoImport {
    val javaModuleName =
      settingKey[String]("The name of the Java (JPMS) module")
    val addModules = settingKey[Seq[String]](
      "Module names that will be added to --add-modules option"
    )
    val moduleDependencies = taskKey[Seq[ModuleID]](
      "Modules dependencies that will be added to --module-path option. List all the sbt modules " +
      "that should be added on module-path. Use it only for external dependencies."
    )

    val internalModuleDependencies = taskKey[Seq[File]](
      """
        |Inter-project JPMS module dependencies. This task has a different return type than
        |`moduleDependencies` task. It returns a sequence of files on purpose - that way,
        |projects are able to override their `exportedModule` task to somehow prepare for
        |modularization.
        |""".stripMargin
    )

    val modulePath = taskKey[Seq[File]](
      "Directories (Jar archives or expanded Jar archives) that will be put into " +
      "--module-path option"
    )
    val patchModules = taskKey[Map[String, Seq[File]]](
      """
        |A map of module names to directories (Jar archives or expanded Jar archives) that will be
        |put into --patch-module option.
        |""".stripMargin
    )
    val addExports = taskKey[Map[String, Seq[String]]](
      """
        |A map of module names to packages that will be put into --add-exports option.
        |The format of `--add-exports` option is `module/package=target-module(,target-module)*`
        |The key in the map is `module/package` and the value is a sequence of target modules
        |""".stripMargin
    )
    val addReads = taskKey[Map[String, Seq[String]]](
      """
        |A map of module names to modules that will be put into --add-reads option.
        |When a module A reads a module B, it means that it "depends" on it - it has the same
        |effect as if module A would have `requires B` in its module-info.java file.
        |""".stripMargin
    )

    val addOpens = taskKey[Map[String, Seq[String]]] (
      """
        |A map of module names with packages to modules that will be put into --add-opens option to java.
        |Note that this option is not added to `javac`, only to `java`.
        |For example `org.enso.runtime/org.enso.my.package=ALL-UNNAMED` will open the package
        |`org.enso.my.package` in the module `org.enso.runtime` to all unnamed modules.
        |Specify it as `addOpens := Map("org.enso.runtime/org.enso.my.package" -> List("ALL-UNNAMED"))`.
        |""".stripMargin
    )

    val exportedModule = taskKey[File](
      """
        |Similarly to `exportedProducts` task, this task returns a file that can be
        |directly put on module-path. For majority of projects, this task will have
        |the same result as `exportedProducts`. The purpose of this task is to be able
        |for the projects to *prepare* for modularization. For example, mixed Scala/Java
        |projects are known to be problematic for modularization, and one needs to manually
        |compile `module-info.java` file. For this mixed project, this task can be declared
        |to depend on `compileModuleInfo`.
        |""".stripMargin
    )

    val exportedModuleBin = taskKey[File](
      "Similar to `packageBin` task. This task returns a modular JAR archive that can be " +
      "directly put on module-path"
    )

    val shouldCompileModuleInfoManually = taskKey[Boolean](
      "If module-info.java should be compiled by us and not by sbt. " +
      "DO NOT USE DIRECTLY."
    )

    val forceModuleInfoCompilation = taskKey[Boolean](
      "Force module-info.java compilation. " +
      "DO NOT USE DIRECTLY."
    )

    val compileModuleInfo = taskKey[Unit](
      "Compiles only module-info.java in some special cases. " +
      "DO NOT USE DIRECTLY."
    )

  }

  import autoImport._

  override lazy val projectSettings: Seq[Setting[_]] = {
    // All the settings are scoped for Compile and Test
    Seq(Compile, Test).flatMap { config: Configuration =>
      Seq(
        config / addModules := Seq.empty,
        config / moduleDependencies := Seq.empty,
        config / internalModuleDependencies := Seq.empty,
        config / shouldCompileModuleInfoManually := {
          val javaSrcDir = (config / javaSource).value
          val modInfo =
            javaSrcDir.toPath.resolve("module-info.java").toFile
          val hasModInfo = modInfo.exists
          val projName = moduleName.value
          val logger = streams.value.log
          val hasScalaSources = (config / scalaSource).value.exists()
          val _compileOrder = (config / compileOrder).value
          val res =
            _compileOrder == CompileOrder.Mixed &&
              hasModInfo &&
              hasScalaSources
          if (res) {
            logger.debug(
              s"[JPMSPlugin] Project '$projName' will have `module-info.java` compiled " +
                "manually. If this is not the intended behavior, consult the documentation " +
                "of JPMSPlugin."
            )
          }
          // Check excludeFilter - there should be module-info.java specified
          if (res && !excludeFilter.value.accept(modInfo)) {
            logger.error(
              s"[JPMSPlugin/$projName] `module-info.java` is not in `excludeFilter`. " +
                "You should add module-info.java to " +
                "`excludedFilter` so that sbt does not handle the compilation. Check docs of JPMSPlugin."
            )
          }
          res
        },
        // module-info.java compilation will be forced iff there are no other Java sources except
        // for module-info.java.
        config / forceModuleInfoCompilation := Def.taskIf {
          if ((config / shouldCompileModuleInfoManually).value) {
            val javaSources = (config / unmanagedSources).value
              .filter(_.getName.endsWith(".java"))
            // If there are no Java source in `unmanagedSources`, it means that sbt will
            // not call Java compiler. So we force it to compile `module-info.java`.
            javaSources.isEmpty
          } else {
            false
          }
        }.value,
        config / compileModuleInfo := Def.taskIf {
          if ((config / forceModuleInfoCompilation).value) {
            JPMSUtils.compileModuleInfo().value
          } else {
            // nop
            ()
          }
        }.value,
        // modulePath is set based on `moduleDependencies` and `internalModuleDependencies`
        config / modulePath := {
          // Do not use fullClasspath here - it will result in an infinite recursion
          // and sbt will not be able to detect the cycle.
          transformModuleDependenciesToModulePath(
            (config / moduleDependencies).value,
            (config / internalModuleDependencies).value,
            (config / dependencyClasspath).value,
            streams.value.log,
            moduleName.value
          )
        },
        // Returns the reference to target/classes directory and ensures that module-info
        // is compiled and present in the target directory.
        config / exportedModule := Def
          .task {
            val targetClassDir = (config / exportedProducts).value
              .map(_.data)
              .head
            val logger = streams.value.log
            val projName = moduleName.value
            if (!isModule(targetClassDir)) {
              logger.error(
                s"[JPMSPlugin/$projName] The target classes directory ${targetClassDir.getAbsolutePath} is not " +
                  "a module - it does not contain module-info.class. Make sure the `compileModuleInfo` task " +
                  "is set correctly."
              )
            }
            targetClassDir
          }
          .dependsOn(config / compileModuleInfo)
          .dependsOn(config / compile)
          .value,
        config / exportedModuleBin := {
          (config / packageBin)
            .dependsOn(config / exportedModule)
            .value
        },
        config / patchModules := Map.empty,
        config / addExports := Map.empty,
        config / addReads := Map.empty,
        config / addOpens := Map.empty,
        // No --add-opens option to javac
        config / javacOptions ++= {
          constructOptions(
            streams.value.log,
            moduleName.value,
            (config / modulePath).value,
            (config / addModules).value,
            (config / patchModules).value,
            (config / addExports).value,
            (config / addReads).value
          )
        },
        config / javaOptions ++= {
          constructOptions(
            streams.value.log,
            moduleName.value,
            (config / modulePath).value,
            (config / addModules).value,
            (config / patchModules).value,
            (config / addExports).value,
            (config / addReads).value,
            (config / addOpens).value
          )
        },
        // Sanitize cmd line arguments
        config / javacOptions := joinModulePathOption((config / javacOptions).value),
        config / javaOptions := joinModulePathOption((config / javaOptions).value)
      )
    }
  }

  /** @param moduleDeps External module dependencies, fetched from `moduleDependencies` task.
    * @param classPath Dependency class path of the project. From this class path, external dependencies
    *                  will be searched for.
    * @param internalModuleDeps Internal module dependencies, fetched from `internalModuleDependencies` task.
    *                           It is assumed that there is `module-info.class` in the root of the internal
    *                           module dependency.
    * @param logger
    * @param currProjName Current name of the local project, for debugging purposes.
    * @return
    */
  private def transformModuleDependenciesToModulePath(
    moduleDeps: Seq[ModuleID],
    internalModuleDeps: Seq[File],
    classPath: Def.Classpath,
    logger: ManagedLogger,
    currProjName: String
  ): Seq[File] = {
    moduleDeps.foreach { moduleDep =>
      if (moduleDep.organization == "org.enso") {
        logger.warn(
          s"[JPMSPlugin/$currProjName] ModuleID $moduleDep specified inside " +
          "`moduleDependencies` task. This is and internal dependency " +
          "and should be specified in `internalModuleDependencies`. "
        )
      }
    }

    internalModuleDeps.foreach { internalModuleDep =>
      if (internalModuleDep.isDirectory) {
        val modInfo =
          internalModuleDep.toPath.resolve("module-info.class").toFile
        if (!modInfo.exists()) {
          logger.warn(
            s"[JPMSPlugin/$currProjName] Internal module dependency $internalModuleDep does not contain " +
            "module-info.class file. Ensure it is an automatic module."
          )
        }
      } else if (internalModuleDep.getName.endsWith(".jar")) {
        val jarFile      = new JarFile(internalModuleDep)
        val modInfoEntry = jarFile.getJarEntry("module-info.class")
        if (modInfoEntry == null) {
          logger.warn(
            s"[JPMSPlugin/$currProjName] Internal module dependency (JAR) $internalModuleDep does not contain " +
            "module-info.class file. Ensure it is an automatic module."
          )
        }
      } else {
        logger.error(
          s"[JPMSPlugin/$currProjName] Internal module dependency $internalModuleDep is not a directory " +
          "nor a jar file. This is not supported. "
        )
      }
    }

    val cp = JPMSUtils.filterModulesFromClasspath(
      classPath,
      moduleDeps,
      logger,
      currProjName,
      shouldContainAll = true
    )
    val externalFiles = cp.map(_.data)
    externalFiles ++ internalModuleDeps
  }

  private def isModule(file: File): Boolean = {
    if (file.isDirectory) {
      val modInfo = file.toPath.resolve("module-info.class").toFile
      modInfo.exists()
    } else if (file.getName.endsWith(".jar")) {
      val jarFile      = new JarFile(file)
      val modInfoEntry = jarFile.getJarEntry("module-info")
      modInfoEntry == null
    } else {
      false
    }
  }

  private def constructOptions(
    log: Logger,
    curProjName: String,
    modulePath: Seq[File],
    addModules: Seq[String]              = Seq.empty,
    patchModules: Map[String, Seq[File]] = Map.empty,
    addExports: Map[String, Seq[String]] = Map.empty,
    addReads: Map[String, Seq[String]]   = Map.empty,
    addOpens: Map[String, Seq[String]]   = Map.empty
  ): Seq[String] = {
    val patchOpts: Seq[String] = patchModules.flatMap {
      case (moduleName, dirsToPatch) =>
        val patchStr = dirsToPatch
          .map(_.getAbsolutePath)
          .mkString(File.pathSeparator)
        Seq(
          "--patch-module",
          s"$moduleName=$patchStr"
        )
    }.toSeq

    val addExportsOpts: Seq[String] = addExports.flatMap {
      case (modPkgName, targetModules) =>
        if (!modPkgName.contains("/")) {
          log.error(
            s"[JPMSPlugin/$curProjName] Invalid module/package name: $modPkgName " +
            "in `addExports` task."
          )
        }
        Seq(
          "--add-exports",
          modPkgName + "=" + targetModules.mkString(",")
        )
    }.toSeq

    val modulePathOpts = if (modulePath.isEmpty) {
      Seq.empty
    } else {
      Seq(
        "--module-path",
        modulePath.map(_.getAbsolutePath).mkString(File.pathSeparator)
      )
    }

    val addModsOpts = if (addModules.isEmpty) {
      Seq.empty
    } else {
      Seq(
        "--add-modules",
        addModules.mkString(",")
      )
    }

    val addReadsOpts = addReads.flatMap { case (modName, targetModules) =>
      Seq(
        "--add-reads",
        modName + "=" + targetModules.mkString(",")
      )
    }.toSeq

    val addOpensOpts = addOpens.flatMap { case (modPkgName, targetModules) =>
      if (!modPkgName.contains("/")) {
        log.error(
          s"[JPMSPlugin/$curProjName] Invalid module/package name: $modPkgName " +
          "in `addOpens` task."
        )
      }
      Seq(
        "--add-opens",
        modPkgName + "=" + targetModules.mkString(",")
      )
    }.toSeq

    modulePathOpts ++ addModsOpts ++ patchOpts ++ addExportsOpts ++ addReadsOpts ++ addOpensOpts
  }

  /**
   * Searches for multiple `--module-path` cmd line options and joins them into a single
   * option.
   * If there are multiple `--module-path` options passed to `java` or `javac`, only the
   * last one specified is considered.
   * Note that this is not an issue for other JPMS-related cmd line options, like
   * `--add-modules`
   * @param opts Current value of cmd line options
   * @return
   */
  private def joinModulePathOption(
    opts: Seq[String]
  ): Seq[String] = {
    val modulePathOpt = new StringBuilder()
    val optIdxToRemove = mutable.HashSet[Int]()
    // Find all `--module-path` options and join them into a single option
    for ((opt, idx) <- opts.zipWithIndex) {
      if (opt == "--module-path" || opt == "-p") {
        optIdxToRemove += idx
        optIdxToRemove += idx + 1
        modulePathOpt.append(opts(idx + 1))
        modulePathOpt.append(":")
      }
    }

    if (modulePathOpt.nonEmpty) {
      // Remove the last colon
      modulePathOpt.deleteCharAt(modulePathOpt.length - 1)
      val newOpts = mutable.ArrayBuffer[String]()
      for ((opt, idx) <- opts.zipWithIndex) {
        if (!optIdxToRemove.contains(idx)) {
          newOpts += opt
        }
      }
      Seq(
        "--module-path",
        modulePathOpt.toString
      ) ++ newOpts
    } else {
      opts
    }
  }
}
