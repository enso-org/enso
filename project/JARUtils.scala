import sbt.{IO, Tracked}
import sbt.std.Streams
import sbt.util.{CacheStoreFactory, FileInfo}

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarFile, JarOutputStream}
import scala.util.{Try, Using}

object JARUtils {

  /** Extracts all file entries starting with `extractPrefix` from `inputJarPath` to `extractedFilesDir`,
    * optionally renaming them with `renameFunc`.
    * If `outputJarPath` is not empty, the remaining contents of the jar is copied into `outputJarPath`.
    *
    * @param inputJarPath      Path to the JAR archive. Will not be modified.
    * @param extractPrefix     Optional prefix of the files to extract.
    * @param outputJarPath     Optional path to the output JAR. Input JAR will be copied here without the files
    *                          starting with `extractPrefix`.
    * @param extractedFilesDir Destination directory for the extracted files. The prefix from the
    *                          extracted files is tripped.
    * @param renameFunc        Function that renames the extracted files. The extracted file name is taken
    *                          from the jar entry, and thus may contain slashes. If None is returned, the
    *                          file is ignored and not extracted.
    */
  def extractFilesFromJar(
    inputJarPath: Path,
    extractPrefix: Option[String],
    outputJarPath: Option[Path],
    extractedFilesDir: Path,
    renameFunc: String => Option[String],
    logger: sbt.util.Logger,
    cacheStoreFactory: CacheStoreFactory,
    cleanOutputDirs: Boolean = true
  ): Unit = {
    val dependencyStore = cacheStoreFactory.make("extract-jar-files")
    // Make sure that the actual file extraction is done only iff some of the cached files change.
    val cachedFiles = Set(
      inputJarPath.toFile
    )
    var shouldExtract = false
    Tracked.diffInputs(dependencyStore, FileInfo.hash)(cachedFiles) { report =>
      shouldExtract =
        report.modified.nonEmpty || report.removed.nonEmpty || report.added.nonEmpty
    }

    if (!shouldExtract) {
      logger.debug("No changes in the input JAR, skipping extraction.")
      return
    } else {
      logger.info(
        s"Extracting files with prefix '${extractPrefix}' from $inputJarPath to $extractedFilesDir."
      )
    }

    if (cleanOutputDirs) {
      outputJarPath.foreach(outputJarPath =>
        ensureDirExistsAndIsClean(outputJarPath.getParent, logger)
      )
      ensureDirExistsAndIsClean(extractedFilesDir, logger)
    }
    Using(new JarFile(inputJarPath.toFile)) { inputJar =>
      outputJarPath match {
        case Some(outputJarPath) =>
          Using(new JarOutputStream(Files.newOutputStream(outputJarPath))) {
            outputJar =>
              inputJar.stream().forEach { entry =>
                if (
                  (extractPrefix.isEmpty || entry.getName
                    .startsWith(extractPrefix.get)) && !entry.isDirectory
                ) {
                  renameFunc(entry.getName) match {
                    case Some(strippedEntryName) =>
                      assert(!strippedEntryName.startsWith("/"))
                      val destFile =
                        extractedFilesDir.resolve(strippedEntryName)
                      if (!destFile.getParent.toFile.exists) {
                        Files.createDirectories(destFile.getParent)
                      }
                      Using(inputJar.getInputStream(entry)) { is =>
                        Files.copy(is, destFile)
                      }.recover({ case e: IOException =>
                        logger.err(
                          s"Failed to extract $entry to $destFile: ${e.getMessage}"
                        )
                        e.printStackTrace(System.err)
                      })
                    case None =>
                      if (entry.getName.endsWith(".class")) {
                        outputJar.putNextEntry(new JarEntry(entry.getName))
                        Using(inputJar.getInputStream(entry)) { is =>
                          is.transferTo(outputJar)
                        }.recover({ case e: IOException =>
                          logger.err(
                            s"Failed to copy $entry to output JAR: ${e.getMessage}"
                          )
                          e.printStackTrace(System.err)
                        })
                        outputJar.closeEntry()
                      }
                  }
                } else {
                  outputJar.putNextEntry(new JarEntry(entry.getName))
                  Using(inputJar.getInputStream(entry)) { is =>
                    is.transferTo(outputJar)
                  }.recover({ case e: IOException =>
                    logger.err(
                      s"Failed to copy $entry to output JAR: ${e.getMessage}"
                    )
                    e.printStackTrace(System.err)
                  })
                  outputJar.closeEntry()
                }
              }
          }.recover({ case e: IOException =>
            logger.err(
              s"Failed to create output JAR at $outputJarPath: ${e.getMessage}"
            )
            e.printStackTrace(System.err)
          })
        case None =>
          inputJar.stream().forEach { entry =>
            if (
              (extractPrefix.isEmpty || entry.getName
                .startsWith(extractPrefix.get)) && !entry.isDirectory
            ) {
              renameFunc(entry.getName) match {
                case Some(strippedEntryName) =>
                  assert(!strippedEntryName.startsWith("/"))
                  val destFile = extractedFilesDir.resolve(strippedEntryName)
                  if (!destFile.getParent.toFile.exists) {
                    Files.createDirectories(destFile.getParent)
                  }
                  Using(inputJar.getInputStream(entry)) { is =>
                    Files.copy(is, destFile)
                  }.recover({ case e: IOException =>
                    logger.err(
                      s"Failed to extract $entry to $destFile: ${e.getMessage}"
                    )
                    e.printStackTrace(System.err)
                  })
                case None => ()
              }
            }
          }
      }
    }.recover({ case e: IOException =>
      logger.err(
        s"Failed to extract files from $inputJarPath to $extractedFilesDir: ${e.getMessage}"
      )
      e.printStackTrace(System.err)
    })
  }

  private def ensureDirExistsAndIsClean(
    path: Path,
    logger: sbt.util.Logger
  ): Unit = {
    require(path != null)
    val dir = path.toFile
    if (dir.exists && dir.isDirectory) {
      // Clean previous contents
      IO.delete(IO.listFiles(dir))
    } else {
      try {
        IO.createDirectory(dir)
      } catch {
        case e: IOException =>
          logger.err(
            s"Failed to create directory $path: ${e.getMessage}"
          )
          e.printStackTrace(System.err)
      }
    }
  }
}
