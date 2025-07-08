import sbt.*
import sbt.util.CacheStoreFactory

object EngineNativeLibraryExtractor {
  private val JLINE_NATIVE = "jline-native"

  /** Extracts native libraries from engine jars (all transitive dependencies of
    * engine).
    *
    * For every JAR file for which native libraries should be extracted:
    * <ul>
    *   <li>Extracts the native libraries into the `componentDir` directory.</li>
    *   <li>Copies the rest of the JAR archive into a new JAR file with the
    *   `-thin.jar` suffix, which contains only the non-native parts of the JAR.</li>
    *   <li>Deletes the original (fat) JAR file from the `componentDir` directory.</li>
    * </ul>
    * @param componentDir Component directory where all the dependencies of engine are stored.
    * @param updateReport Update report for the "engine-runner" project.
    *            Get it with `(engine-runner/update).value`.
    * @param scalaBinaryVersion
    * @param cacheFactory
    * @param previousRun
    * @return
    */
  def extractNativeLibraries(
    componentDir: File,
    logger: Logger,
    updateReport: UpdateReport,
    scalaBinaryVersion: String,
    cacheFactory: CacheStoreFactory,
    previousRun: Option[AnalysisOfExtractedNativeLibs]
  ): AnalysisOfExtractedNativeLibs = {
    val jlineNativeJars = JPMSUtils.filterModulesFromUpdate(
      updateReport,
      Dependencies.jlineNative,
      logger,
      "engine-runner",
      scalaBinaryVersion,
      shouldContainAll = true
    )
    require(
      jlineNativeJars.size == 1,
      s"Expected exactly one jline-native jar, found: ${jlineNativeJars}."
    )
    val jlineNativeJar = jlineNativeJars.head
    var analysis = previousRun
      .getOrElse(AnalysisOfExtractedNativeLibs(Map.empty))
    if (!componentDir.exists()) {
      logger.error(
        s"Component directory [$componentDir] does not exist yet. " +
        "It must have been created with createEnginePackageNoIndex task."
      )
      throw new IllegalStateException("Component directory does not exist.")
    }
    logger.debug(
      s"[EngineNativeLibraryExtractor] Extracting native libraries. " +
      s"jlineNativeJar: $jlineNativeJar, analysis; $analysis"
    )

    getFromCache(jlineNativeJar, analysis) match {
      case Some(cachedLibs) =>
        logger.debug(
          "jline-native libraries already extracted, skipping extraction."
        )
      case None =>
        val outJarName = jlineNativeJar.getName.replace(
          ".jar",
          "-thin.jar"
        )
        val outJar = componentDir / outJarName
        if (outJar.exists()) {
          // Extraction is not cache, but the output jar exist. We must first delete it.
          // This can happen if sbt was restarted and the outputJar was not deleted.
          IO.delete(outJar)
        }
        val extractedLibs = extractJLineNative(
          jlineNativeJar,
          outJar,
          componentDir,
          logger,
          cacheFactory,
          analysis
        )
        analysis = analysis.appended(
          AnalysisOfExtractedNativeLibs(
            from        = jlineNativeJar,
            dynamicLibs = extractedLibs,
            thinTarget  = Some(outJar)
          )
        )
    }
    // Delete the old fat jar.
    // It is possible that it does not exist, in that case, just ignore the error.
    deleteFromComponentDir(
      componentDir,
      jlineNativeJar
    )
    analysis
  }

  private def getFromCache(
    jar: File,
    prevRun: AnalysisOfExtractedNativeLibs
  ): Option[List[File]] = {
    prevRun.forJar(jar) match {
      case None => None
      case Some(summary) =>
        if (summary.isOutdated) {
          None
        } else {
          Some(summary.dynamicLibs)
        }
    }
  }

  private def extractJLineNative(
    jLineJar: File,
    outThinJar: File,
    componentDir: File,
    logger: Logger,
    cacheFactory: CacheStoreFactory,
    previousRun: AnalysisOfExtractedNativeLibs
  ): List[File] = {
    val (expectedLibEntry, renameTo) =
      if (Platform.isLinux && Platform.isAmd64) {
        ("Linux/x86_64/libjlinenative.so", "libjlinenative.so")
      } else if (Platform.isLinux && Platform.isArm64) {
        ("Linux/arm64/libjlinenative.so", "libjlinenative.so")
      } else if (Platform.isWindows && Platform.isAmd64) {
        ("Windows/x86_64/jlinenative.dll", "jlinenative.dll")
      } else if (Platform.isMacOS && Platform.isAmd64) {
        ("Mac/x86_64/libjlinenative.jnilib", "libjlinenative.dylib")
      } else if (Platform.isMacOS && Platform.isArm64) {
        ("Mac/arm64/libjlinenative.jnilib", "libjlinenative.dylib")
      } else {
        throw new RuntimeException(
          s"Unsupported platform for JLine native library: ${StdBits.plainOsName()}"
        )
      }

    val prefix = "org.jline.nativ"

    def renameFunc(name: String): Option[String] = {
      val strippedEntryName = name.substring(prefix.length + 1)
      if (strippedEntryName == expectedLibEntry) {
        Some(renameTo)
      } else {
        None
      }
    }

    JARUtils
      .extractFilesFromJar(
        inputJarPath      = jLineJar.toPath,
        outputJarPath     = Some(outThinJar.toPath),
        extractPrefix     = None,
        extractedFilesDir = componentDir.toPath,
        renameFunc        = renameFunc,
        logger            = logger,
        cacheStoreFactory = cacheFactory,
        previousRun       = previousRun.forJar(jLineJar),
        copyMetaInf       = true
      )
      .get
  }

  /** @param jarDependency JAR file that resides somewhere in `$HOME/.cache/coursier`
    */
  private def deleteFromComponentDir(
    componentDir: File,
    jarDependency: File
  ): Unit = {
    val path = componentDir.toPath.resolve(jarDependency.getName)
    IO.delete(path.toFile)
  }
}
