import sbt.util.Logger

import java.io.{File, IOException}
import java.nio.file.{Files, Path}
import java.util.jar.{JarEntry, JarFile, JarOutputStream}
import scala.util.Using

object JarExtract {
  trait JarVisitor {
    def visitJarEntry(
      entry: JarEntry
    ): JarVisitorResult
  }

  sealed trait JarVisitorResult

  case object CopyToOutputJar extends JarVisitorResult

  case object Skip extends JarVisitorResult

  case class CopyToExtractDir(
    rename: Option[String] = None
  ) extends JarVisitorResult

  /** Traverses all the entries in the input JAR file and extracts files
    * according to the provided `visitor` rules.
    * The extracted files are copied to the `extractedFilesDir`, and the
    * output JAR is created at `outputJarFile`.
    *
    * Directory entries are not visited.
    *
    * @param jarFile Input Jar file. Is not modified.
    * @param extractedFilesDir Destination directory for extracted files.
    * @param outputJarFile Destination of the thin output jar
    * @param visitor
    * @param logger
    * @return List of extracted files to the `extractedFilesDir`.
    */
  def extract(
    jarFile: Path,
    extractedFilesDir: Path,
    outputJarFile: Path,
    visitor: JarVisitor,
    logger: Logger
  ): Seq[File] = {
    val extractedFiles = scala.collection.mutable.ListBuffer.empty[File]
    Using(new JarFile(jarFile.toFile)) { inputJar =>
      Using(new JarOutputStream(Files.newOutputStream(outputJarFile))) {
        outputJar =>
          val entries = inputJar.entries()
          while (entries.hasMoreElements) {
            val entry     = entries.nextElement()
            val entryName = entry.getName
            if (!entry.isDirectory) {
              visitor.visitJarEntry(entry) match {
                case Skip =>
                  logger.debug(s"Skipping entry: ${entryName}")
                case CopyToOutputJar =>
                  copyEntry(outputJar, inputJar, entry, logger)
                case CopyToExtractDir(rename) =>
                  val destPath = rename match {
                    case Some(newName) => extractedFilesDir.resolve(newName)
                    case None          => extractedFilesDir.resolve(entryName)
                  }
                  copyEntry(destPath, inputJar, entry, logger)
                  extractedFiles.append(destPath.toFile)
              }
            }
          }
      }.recover({ case e: IOException =>
        logger.err(
          s"Failed to write to output JAR file at $outputJarFile: ${e.getMessage}"
        )
      })
    }.recover({ case e: IOException =>
      logger.err(
        s"Failed to open JAR file at $jarFile: ${e.getMessage}"
      )
    })
    extractedFiles
  }

  private def copyEntry(
    dest: JarOutputStream,
    src: JarFile,
    entry: JarEntry,
    logger: Logger
  ): Unit = {
    dest.putNextEntry(entry)
    Using(src.getInputStream(entry)) { is =>
      is.transferTo(dest)
    }.recover({ case e: IOException =>
      logger.error(
        s"Failed to copy entry ${entry.getName} from JAR: ${e.getMessage}"
      )
      e.printStackTrace(System.err)
    })
    dest.closeEntry()
    logger.debug(
      s"Copied entry ${entry.getName} to output JAR."
    )
  }

  private def copyEntry(
    dest: Path,
    src: JarFile,
    entry: JarEntry,
    logger: Logger
  ): Unit = {
    if (!dest.getParent.toFile.exists) {
      Files.createDirectories(dest.getParent)
    }
    Using(src.getInputStream(entry)) { is =>
      Files.copy(is, dest)
    }.recover({ case e: IOException =>
      logger.error(
        s"Failed to copy entry ${entry.getName} to $dest: ${e.getMessage}"
      )
      e.printStackTrace(System.err)
    })
    logger.debug(
      s"Copied entry ${entry.getName} to extractFileDir '$dest'."
    )
  }

  val openCVExtractor: JarVisitor = (entry: JarEntry) => {
    val extractPrefix = "nu/pattern/opencv"
    val validOsName   = Platform.osName()
    val validArch     = Platform.arch()
    val entryName     = entry.getName
    if (entryName.startsWith(extractPrefix)) {
      val strippedEntryName = entryName
        .substring(extractPrefix.length + 1)
        .replace("ARMv8", "aarch64")
      if (
        strippedEntryName.contains("linux/ARM") ||
        strippedEntryName.contains("linux/x86_32") ||
        strippedEntryName.contains("README.md")    ||
        // Remove native libs for different platforms
        !strippedEntryName.contains(validOsName) ||
        !strippedEntryName.contains(validArch)
      ) {
        Skip
      } else {
        val rename = strippedEntryName
          .replace("linux/x86_64", "amd64")
          .replace("windows/x86_64", "amd64")
          .replace("windows/x86_32", "x86_32")
          .replace("osx/aarch64", "aarch64")
          .replace("osx/x86_64", "amd64")
        CopyToExtractDir(Some(rename))
      }
    } else {
      CopyToOutputJar
    }
  }
}
