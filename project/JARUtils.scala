import sbt.{IO, Tracked}
import sbt.std.Streams
import sbt.util.{CacheStoreFactory, FileInfo}

import java.io.{File, IOException}
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
    * @param logger SBT's logger
    * @param cacheStoreFactory SBT's cache sotre factory
    * @param previousRun summary of previous extraction data, if available
    * @return list of extracted native libraries
    */
  def extractFilesFromJar(
    inputJarPath: Path,
    extractPrefix: Option[String],
    outputJarPath: Option[Path],
    extractedFilesDir: Path,
    renameFunc: String => Option[String],
    logger: sbt.util.Logger,
    cacheStoreFactory: CacheStoreFactory,
    previousRun: Option[ExtractedNativeLibSummary]
  ): Try[List[File]] = {
    val dependencyStore = cacheStoreFactory.make("extract-jar-files")
    val inputJarFile    = inputJarPath.toFile
    // Make sure that the actual file extraction is done only iff some of the cached files change.
    val cachedFiles = Set(
      inputJarFile
    )
    var shouldExtract = false
    Tracked.diffInputs(dependencyStore, FileInfo.hash)(cachedFiles) { report =>
      shouldExtract =
        report.modified.nonEmpty || report.removed.nonEmpty || report.added.nonEmpty || outputJarPath
          .exists(!_.toFile.exists())
    }

    if (!shouldExtract) {
      logger.debug(
        "No changes in the input JAR `" + inputJarPath + "`, skipping extraction."
      )
      return previousRun match {
        case Some(r) => scala.util.Success(r.dynamicLibs)
        case None =>
          scala.util.Failure(new RuntimeException("No cache information"))
      }
    } else {
      logger.info(
        s"Extracting files with prefix '${extractPrefix}' from $inputJarPath to $extractedFilesDir."
      )
    }

    var dynamicLibs: List[File] = Nil
    Using(new JarFile(inputJarFile)) { inputJar =>
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
                      val destPath =
                        extractedFilesDir.resolve(strippedEntryName)
                      val destFile = destPath.toFile

                      dynamicLibs = destFile :: dynamicLibs
                      if (
                        destFile.exists() && destFile.exists() && inputJarFile
                          .lastModified() < destFile.lastModified()
                      ) {
                        logger.info("File already up-to-date. Skipping...")
                      } else {
                        if (!destPath.getParent.toFile.exists) {
                          Files.createDirectories(destPath.getParent)
                        }
                        Using(inputJar.getInputStream(entry)) { is =>
                          Files.copy(is, destPath)
                        }.recover({ case e: IOException =>
                          logger.err(
                            s"Failed to extract $entry to $destPath: ${e.getMessage}"
                          )
                          e.printStackTrace(System.err)
                        })
                      }
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
              s"Failed to create output JAR at $outputJarPath (parent dir exists: ${outputJarPath.getParent.toFile
                .exists()}): ${e.getMessage}"
            )
            e.printStackTrace(System.err)
            throw e;
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
                  val destPath = extractedFilesDir.resolve(strippedEntryName)
                  val destFile = destPath.toFile
                  dynamicLibs = destFile :: dynamicLibs
                  if (
                    destFile.exists() && destFile.exists() && inputJarFile
                      .lastModified() < destFile.lastModified()
                  ) {
                    logger.info("File already up-to-date. Skipping...")
                  } else {
                    if (!destPath.getParent.toFile.exists) {
                      Files.createDirectories(destPath.getParent)
                    }
                    Using(inputJar.getInputStream(entry)) { is =>
                      Files.copy(is, destPath)
                    }.recover({ case e: IOException =>
                      logger.err(
                        s"Failed to extract $entry to $destFile: ${e.getMessage}"
                      )
                      e.printStackTrace(System.err)
                    })
                  }
                case None => ()
              }
            }
          }
      }
      dynamicLibs
    }.recover({ case e: IOException =>
      logger.err(
        s"Failed to extract files from $inputJarPath to $extractedFilesDir: ${e.getMessage}"
      )
      e.printStackTrace(System.err)
      Nil
    })
  }
}
