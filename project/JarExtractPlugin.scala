import sbt.*
import sbt.Keys.*
import sbt.AutoPlugin

import java.nio.file.Files
import scala.jdk.CollectionConverters.asScalaBufferConverter

/** This plugin is meant to be used by projects that are merely wrappers for thin jars.
  * The project has exactly one input jar, and has two outputs:
  * - thin jar with some files copied from the input jar.
  * - directory with extracted files from the input jar.
  *
  * `jarExtractor` sets a specific [[JarExtract.JarVisitor]] that instructs the plugin how
  * to extract the files - which entries should be copied to the output jar, and
  * which entries should be extracted to the directory.
  */
object JarExtractPlugin extends AutoPlugin {
  object autoImport {
    val inputJar = settingKey[ModuleID](
      """
        |Input Jar file to extract files from.
        |It is important that this moduleID is also in `libraryDependencies`,
        |otherwise this plugin will not be able to resolve it.
        |""".stripMargin
    )
    val jarExtractor = settingKey[JarExtract.JarVisitor](
      "Jar visitor that defines how to extract files from the input jar"
    )
    // Is implemented by this plugin
    lazy val thinJarOutput = taskKey[File](
      "Output thin jar with some files copied from the input jar"
    )
    // Is implemented by this plugin
    val extractedFiles = taskKey[Seq[File]](
      "Files extracted from the input jar"
    )
  }

  private lazy val inputJarResolved = taskKey[File](
    "Resolved input jar file from the module ID"
  )
  private lazy val extract = taskKey[Unit](
    "Extract files from the input jar using the defined jar visitor"
  )
  private lazy val isExtracted = taskKey[Boolean](
    "Check if the input jar has been extracted - if the outputs are cached"
  )
  private lazy val extractedFilesDir = settingKey[File](
    "Directory where extracted files will be put"
  )
  private lazy val jarOut = settingKey[File](
    "jarOut"
  )

  import autoImport._

  override lazy val projectSettings: Seq[Setting[_]] = Seq(
    inputJarResolved := {
      val modId  = inputJar.value
      val logger = streams.value.log
      val resolvedModules = JPMSUtils.filterModulesFromUpdate(
        (Compile / update).value,
        Seq(modId),
        logger,
        moduleName.value,
        scalaBinaryVersion.value,
        shouldContainAll = true
      )
      if (resolvedModules.size != 1) {
        val errMsg =
          s"Expected exactly one module for ${modId}, but found ${resolvedModules.size}. " +
          "Is it specified in libraryDependencies?"
        logger.error(errMsg)
        throw new IllegalStateException(errMsg)
      }
      resolvedModules.head
    },
    jarOut := {
      val targetDir = (Compile / target).value
      val modName   = moduleName.value
      targetDir / (modName + "-thin.jar")
    },
    extractedFilesDir := {
      val targetDir = (Compile / target).value
      targetDir / "extracted-files"
    },
    extract := {
      val inJar       = inputJarResolved.value
      val outJar      = jarOut.value
      val extFilesDir = extractedFilesDir.value
      val trackedFiles = Set(
        inJar,
        outJar,
        extFilesDir
      )
      val extractor         = jarExtractor.value
      val cacheStoreFactory = streams.value.cacheStoreFactory
      val logger            = streams.value.log
      val modName           = moduleName.value
      val store             = cacheStoreFactory.make("jar-extract")
      Tracked.diffOutputs(store, FileInfo.hash)(trackedFiles) { report =>
        logger.debug("jar-extract report: " + report)
        val outputsExist  = outJar.exists() && extFilesDir.exists()
        val shouldExtract = !outputsExist || report.modified.nonEmpty
        if (shouldExtract) {
          logger.info(
            s"[JarExtractPlugin:$modName] Extracting ${inJar.getName}"
          )
          JarExtract.extract(
            inJar.toPath,
            extFilesDir.toPath,
            outJar.toPath,
            extractor,
            logger
          )
        } else {
          logger.info(
            s"[JarExtractPlugin:$modName] ${inJar.getName} is already extracted"
          )
        }
        outputsExist && report.modified.isEmpty
      }
    },
    extractedFiles := Def
      .task {
        val dir = extractedFilesDir.value
        Files
          .walk(dir.toPath)
          .toList
          .asScala
          .map(_.toFile)
          .filter(_.isFile)
      }
      .dependsOn(extract)
      .value,
    thinJarOutput := Def
      .task {
        jarOut.value
      }
      .dependsOn(extract)
      .value
  )
}
