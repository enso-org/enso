import sbt.util.Logger

import java.io.IOException
import java.nio.file.{FileSystems, Files, Path}
import java.util.jar.{JarEntry, JarFile, JarOutputStream}
import scala.util.Using

/** A jar entry can either be:
  * - Copied to the output (thin) jar file.
  * - Copied to the "extracted files" directory. These are usually native libs.
  * - Skipped.
  *
  * If a jar entry does not match any of the globs, it is skipped.
  * @param mapping Mapping of globs to commands for extracting files from a JAR.
  */
case class JarExtractor(
  mapping: (String, JarExtractor.Command)*
)

object JarExtractor {

  /** All supported native library architectures.
    */
  sealed trait NativeLibArch {
    // Path of the library inside the extracted files directory
    // Inspired by `org.enso.pkg.NativeLibraryFinder`
    def path: String
    // An extension expected when loading libraries from the given architecture
    def extension: String
    // A prefix of the library expected when loading libraries from the given architecture.
    // If None, arch makes no assumptions about the name of the file.
    // If Some(a: Left), then a library must not have a prefix `a` in the file name.
    // If Some(b: Right), then a library must have a prefix `b` in the file name.
    def prefix: Option[Either[String, String]]
  }

  case object LinuxAMD64 extends NativeLibArch {
    override val path: String                           = "amd64/linux"
    override val extension: String                      = "so"
    override val prefix: Option[Either[String, String]] = Some(Right("lib"))
  }
  case object WindowsAMD64 extends NativeLibArch {
    override val path: String                           = "amd64/windows"
    override val extension: String                      = "dll"
    override val prefix: Option[Either[String, String]] = Some(Left("lib"))
  }
  case object MacOSArm64 extends NativeLibArch {
    override val path: String                           = "aarch64/macos"
    override val extension: String                      = "dylib"
    override val prefix: Option[Either[String, String]] = Some(Right("lib"))
  }

  // What to do with the matching jar entries.
  sealed trait Command

  case object CopyToOutputJar extends Command

  /** A command that instructs the extractor that the current jar entry is
    * a native library that should be copied to a `polyglot/lib` directory
    * and a valid target directory hierarchy should be created.
    *
    * For example, if the entry is `foo.so` and the `arch` parameter is
    * [[LinuxAMD64]], the entry will be copied to `amd64/linux/foo.so`.
    *
    * The entry will be copied only if the architecture matches the current
    * platform's architecture.
    *
    * @param arch If specified, will be copied only iff the architecture is
    *        the same as the current platform.
    */
  case class PolyglotLib(
    arch: NativeLibArch
  ) extends Command

  /** Traverses all the entries in the input JAR file and extracts files
    * according to the provided `extractor` rules.
    * The extracted files are copied to the `extractedFilesDir`, and the
    * output JAR is created at `outputJarFile`.
    *
    * @param jarFile Input Jar file. Is not modified.
    * @param polyglotLibDir Destination directory for extracted native libraries.
    * @param outputJarFile Destination of the thin output jar
    */
  def extract(
    jarFile: Path,
    polyglotLibDir: Path,
    outputJarFile: Path,
    extractor: JarExtractor,
    logger: Logger
  ): Unit = {
    require(
      !polyglotLibDir.toFile.exists,
      s"Polyglot lib directory ${polyglotLibDir.toAbsolutePath} already exists."
    )
    require(
      !outputJarFile.toFile.exists,
      s"Output JAR file ${outputJarFile.toAbsolutePath} already exists."
    )
    Using(new JarFile(jarFile.toFile)) { inputJar =>
      Using(new JarOutputStream(Files.newOutputStream(outputJarFile))) {
        outputJar =>
          val entries = inputJar.entries()
          while (entries.hasMoreElements) {
            val entry     = entries.nextElement()
            val entryName = entry.getName
            for ((glob, command) <- extractor.mapping) {
              val pathMatcher =
                FileSystems.getDefault.getPathMatcher("glob:" + glob)
              val entryPath = Path.of(entryName)
              if (pathMatcher.matches(entryPath)) {
                command match {
                  case CopyToOutputJar =>
                    copyEntry(outputJar, inputJar, entry, logger)
                  case PolyglotLib(arch) =>
                    // Silently rename the old `*.jnilib` files to `*.dylib`.
                    val fullPath = entryPath.getFileName.toString
                    val idx      = fullPath.lastIndexOf('.')
                    if (idx > 0) {
                      val oldExtension = fullPath.substring(idx + 1)

                      val fullPath1 =
                        if (oldExtension != arch.extension)
                          fullPath.replace(
                            "." + oldExtension,
                            "." + arch.extension
                          )
                        else fullPath
                      val fullPath2 = arch.prefix
                        .map {
                          case Left(prohibited) =>
                            fullPath1.stripPrefix(prohibited)
                          case Right(required) =>
                            if (fullPath1.startsWith(required)) fullPath1
                            else required + fullPath1
                        }
                        .getOrElse(fullPath1)
                      val destPath = polyglotLibDir
                        .resolve(arch.path)
                        .resolve(fullPath2)
                      if (archMatchesCurPlatform(arch)) {
                        copyEntry(destPath, inputJar, entry, logger)
                      }
                    }
                }
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
  }

  private def archMatchesCurPlatform(
    arch: NativeLibArch
  ): Boolean = {
    val osName = Platform.osName()
    (arch, Platform.osName(), Platform.arch()) match {
      case (LinuxAMD64, "linux", "x86_64")     => true
      case (WindowsAMD64, "windows", "x86_64") => true
      case (MacOSArm64, "osx", "aarch64")      => true
      case _                                   => false
    }
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
}
