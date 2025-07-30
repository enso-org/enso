import sbt.*
import sbt.Keys.*
import sbt.AutoPlugin

import java.nio.file.Files
import scala.jdk.CollectionConverters.asScalaBufferConverter

/** This plugin is meant to be used by projects that are merely wrappers for thin jars.
  * The project has exactly one input jar, and has two outputs:
  * - thin jar with some files copied from the input jar.
  * - directory with extracted files from the input jar.
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
    val jarExtractor = settingKey[JarExtractor](
      "Jar visitor that defines how to extract files from the input jar"
    )
    // Is implemented by this plugin
    lazy val thinJarOutput = taskKey[File](
      "Output thin jar with some files copied from the input jar"
    )
    val extractedFilesDir = taskKey[File](
      "Directory where extracted files will be put"
    )
    val inputJarResolved = taskKey[File](
      "Resolved input jar file from the module ID"
    )
  }

  private lazy val extract = taskKey[Unit](
    "Extract files from the input jar using the defined jar visitor"
  )
  private lazy val thinJarOutputPath = settingKey[File](
    "thinJarOutputPath"
  )
  private lazy val extractedFilesPath = settingKey[File](
    "extractedFilesPath"
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
    thinJarOutputPath := {
      val targetDir = (Compile / target).value
      val modName   = moduleName.value
      targetDir / (modName + "-thin.jar")
    },
    extractedFilesPath := {
      val targetDir = (Compile / target).value
      targetDir / "extracted-files"
    },
    extract := {
      val inJar       = inputJarResolved.value
      val outJar      = thinJarOutputPath.value
      val extFilesDir = extractedFilesPath.value
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
          logger.debug(
            s"[JarExtractPlugin:$modName] Extracting ${inJar.getName}"
          )
          // Ensure that both outputs are deleted before extraction
          IO.delete(outJar)
          IO.delete(extFilesDir)
          JarExtractor.extract(
            inJar.toPath,
            extFilesDir.toPath,
            outJar.toPath,
            extractor,
            logger
          )
        } else {
          logger.debug(
            s"[JarExtractPlugin:$modName] ${inJar.getName} is already extracted"
          )
        }
        outputsExist && report.modified.isEmpty
      }
    },
    extractedFilesDir := Def
      .task {
        extractedFilesPath.value
      }
      .dependsOn(extract)
      .value,
    thinJarOutput := Def
      .task {
        thinJarOutputPath.value
      }
      .dependsOn(extract)
      .value,
    clean := {
      val _ = clean.value
      IO.delete(extractedFilesPath.value)
      IO.delete(thinJarOutputPath.value)
    }
  )
}
