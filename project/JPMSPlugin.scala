import sbt.*
import sbt.Keys.*
import sbt.internal.util.ManagedLogger

import java.io.File
import java.net.{URI, URL}
import java.util.jar.JarFile

/** An automatic plugin that handles everything related to JPMS modules. One needs to explicitly
  * enable this plugin in a project with `.enablePlugins(JPMSPlugin)`. The keys and tasks provided by this plugin
  * corresponds to the module-related options of `javac` and `java` commands.
  *
  * This plugin injects all the module-specific options to `javaOptions`, based on
  * the settings of this plugin.
  *
  * If this plugin is enabled, and no settings/tasks from this plugin are used, then the plugin will
  * not inject anything into `javaOptions` or `javacOptions`.
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

    // TODO: Make this private
    val compileModuleInfo = taskKey[Unit]("Compile module-info.java")

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

    /**
     * Should module-info.java be compiled manually? True iff there is `module-info.java`
     * in java sources and if the compile order is Mixed. In such case, sbt tries to first
     * parse all the Java sources via its internal parser, and that fails for `modue-info`.
     * In these cases, we need to exclude `module-info.java` from the sources and compile it
     * manually.
     *
     * WARNING: Do not use override this task directly if you don't know exactly what you are doing.
     */
    val shouldCompileModuleInfoManually = taskKey[Boolean](
      "Should module-info.java be compiled manually?"
    )
  }

  import autoImport._


  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    addModules := Seq.empty,
    moduleDependencies := Seq.empty,
    internalModuleDependencies := Seq.empty,
    shouldCompileModuleInfoManually := {
      val javaSrcDir = (Compile / javaSource).value
      val hasModInfo =
        javaSrcDir.toPath.resolve("module-info.java").toFile.exists()
      val projName        = moduleName.value
      val logger          = streams.value.log
      val hasScalaSources = (Compile / scalaSource).value.exists()
      val _compileOrder   = (Compile / compileOrder).value
      val res =
        _compileOrder == CompileOrder.Mixed &&
        hasModInfo &&
        hasScalaSources
      if (res) {
        logger.warn(
          s"[JPMSPlugin] Project '$projName' will have `module-info.java` compiled " +
          "manually. If this is not the intended behavior, consult the documentation " +
          "of JPMSPlugin."
        )
      }
      res
    },
    // modulePath is set based on moduleDependencies
    modulePath := {
      // Do not use fullClasspath here - it will result in an infinite recursion
      // and sbt will not be able to detect the cycle.
      transformModuleDependenciesToModulePath(
        (Compile / moduleDependencies).value,
        (Compile / internalModuleDependencies).value,
        (Compile / dependencyClasspath).value,
        streams.value.log,
        moduleName.value
      )
    },
    exportedModule := {
      // Ensure module-info.java is compiled
      compileModuleInfo.value
      val logger   = streams.value.log
      val projName = moduleName.value
      val targetClassDir = (Compile / exportedProducts).value
        .map(_.data)
        .head
      if (!isModule(targetClassDir)) {
        logger.error(
          s"[JPMSPlugin/$projName] The target classes directory ${targetClassDir.getAbsolutePath} is not " +
          "a module - it does not contain module-info.class. Make sure the `compileModuleInfo` task " +
          "is set correctly."
        )
      }
      targetClassDir
    },
    exportedModuleBin := {
      exportedModule.value
      (Compile / packageBin).value
    },
    patchModules := Map.empty,
    addExports := Map.empty,
    addReads := Map.empty,
    compileModuleInfo := Def.taskIf {
      if (shouldCompileModuleInfoManually.value) {
        val projectName = moduleName.value
        val logger      = streams.value.log
        val sources     = (Compile / unmanagedSources).value
        val moduleInfo  = sources.find(_.name == "module-info.java")
        if (moduleInfo.isDefined) {
          logger.error(
            s"[JPMSPlugin/$projectName] module-info.java is contained in `Compile / unmanagedSources`. " +
            s"This means that it is not excluded from the default sbt compilation. " +
            """Declare `excludedFilter := excludedFilter.value || \"module-info.java\"` in settings. """ +
            s"Otherwise, JPMSPlugin cannot manually compile module-info.java. " +
            s"(See the docs for `JPMSPlugin.shouldCompileModuleInfoManually`)"
          )
        }
        JPMSUtils
          .compileModuleInfo()
          .dependsOn(Compile / compile)
          .value
      } else {
        (Compile / compile).value
      }
    }.value,
    // javacOptions only inject --module-path and --add-modules, not the rest of the
    // options.
    Compile / javacOptions ++= {
      constructOptions(
        streams.value.log,
        modulePath = (Compile / modulePath).value,
        addModules = (Compile / addModules).value
      )
    },
    Compile / javaOptions ++= {
      constructOptions(
        streams.value.log,
        (Compile / modulePath).value,
        (Compile / addModules).value,
        (Compile / patchModules).value,
        (Compile / addExports).value,
        (Compile / addReads).value
      )
    },
    Test / modulePath := {
      transformModuleDependenciesToModulePath(
        (Test / moduleDependencies).value,
        (Test / internalModuleDependencies).value,
        (Test / dependencyClasspath).value,
        streams.value.log,
        moduleName.value
      )
    },
    Test / javacOptions ++= {
      constructOptions(
        streams.value.log,
        modulePath = (Test / modulePath).value,
        addModules = (Test / addModules).value
      )
    },
    Test / javaOptions ++= {
      constructOptions(
        streams.value.log,
        (Test / modulePath).value,
        (Test / addModules).value,
        (Test / patchModules).value,
        (Test / addExports).value,
        (Test / addReads).value
      )
    }
  )

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
          logger.error(
            s"[JPMSPlugin/$currProjName] Internal module dependency $internalModuleDep does not contain " +
            "module-info.class file. This is required for JPMS modules."
          )
        }
      } else if (internalModuleDep.getName.endsWith(".jar")) {
        val jarFile      = new JarFile(internalModuleDep)
        val modInfoEntry = jarFile.getJarEntry("module-info.class")
        if (modInfoEntry == null) {
          logger.error(
            s"[JPMSPlugin/$currProjName] Internal module dependency (JAR) $internalModuleDep does not contain " +
            "module-info.class file. This is required for JPMS modules."
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

  def constructOptions(
    log: Logger,
    modulePath: Seq[File],
    addModules: Seq[String]              = Seq.empty,
    patchModules: Map[String, Seq[File]] = Map.empty,
    addExports: Map[String, Seq[String]] = Map.empty,
    addReads: Map[String, Seq[String]]   = Map.empty
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
          log.error(s"JPMSPlugin: Invalid module/package name: $modPkgName")
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

    modulePathOpts ++ addModsOpts ++ patchOpts ++ addExportsOpts ++ addReadsOpts
  }
}
