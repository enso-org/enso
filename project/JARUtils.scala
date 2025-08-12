import sbt.{IO, Tracked}
import sbt.std.Streams
import sbt.util.{CacheStoreFactory, FileInfo}

import java.io.{File, IOException}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.jar.{JarEntry, JarFile, JarOutputStream}
import scala.util.{Try, Using}

object JARUtils {

  /** Removes the specified list of entries from the JAR archive at `jarPath`.
    * Changes the JAR archive in place.
    * If some entries are not found, they are ignored.
    * @param jarPath
    * @param shouldBeDeleted A function that takes the entry name and returns true if the entry should be deleted.
    */
  def removeEntriesFromJar(
    jarPath: Path,
    shouldBeDeleted: String => Boolean
  ): Unit = {
    val tempJarPath = Files.createTempFile("temp-", ".jar")
    Using(new JarFile(jarPath.toFile)) { jarFile =>
      Using(new JarOutputStream(Files.newOutputStream(tempJarPath))) {
        outputJar =>
          jarFile.stream().forEach { entry =>
            if (!shouldBeDeleted(entry.getName)) {
              outputJar.putNextEntry(new JarEntry(entry.getName))
              Using(jarFile.getInputStream(entry)) { is =>
                is.transferTo(outputJar)
              }.recover({ case e: IOException =>
                throw new RuntimeException(
                  s"Failed to copy $entry to output JAR: ${e.getMessage}",
                  e
                )
              })
              outputJar.closeEntry()
            }
          }
      }
    }
    Files.move(
      tempJarPath,
      jarPath,
      StandardCopyOption.REPLACE_EXISTING
    )
    IO.delete(tempJarPath.toFile)
  }

  /** Reads the `Bundle-NativeCode` entries from the JAR manifest.
    * See <a href="https://docs.osgi.org/specification/osgi.core/8.0.0/framework.module.html#framework.module-loading.native.code.libraries">
    *   OSGi Bundle-NativeCode specification
    * </a>
    *
    * If there is no such manifest attribute, an empty list is returned.
    */
  def readNativeCodeEntriesFromManifest(
    jarPath: Path
  ): List[NativeCodeEntry] =
    Using(new JarFile(jarPath.toFile)) { jarFile =>
      val parsedHeader = for {
        manifest <- Option(jarFile.getManifest)
        nativeCodeHeader <- Option(
          manifest.getMainAttributes.getValue("Bundle-NativeCode")
        )
      } yield nativeCodeHeader.split(",").map(_.trim).toList

      parsedHeader
        .map(entries =>
          entries.flatMap { entry =>
            val parsed = NativeCodeEntry.parseFromEntry(entry)
            // `parsedFromEntry` should return Either but this will do
            if (parsed.isEmpty) {
              throw new IllegalStateException(
                s"Invalid Bundle-NativeCode entry: $entry"
              )
            }
            parsed
          }
        )
    }.toOption.flatten.getOrElse(Nil)

  /** @param processor See `processor` in <a href="https://docs.osgi.org/specification/osgi.core/8.0.0/framework.module.html#framework.module-loading.native.code.libraries">
    *                  OSGi Bundle-NativeCode specification
    *                  </a>
    * @param osName See `osname` in <a href="https://docs.osgi.org/specification/osgi.core/8.0.0/framework.module.html#framework.module-loading.native.code.libraries">
    *                 OSGi Bundle-NativeCode specification
    *                 </a>
    * @param libPath Path inside the JAR
    */
  case class NativeCodeEntry(
    processor: String,
    osName: String,
    libPath: String
  ) {
    def isValid: Boolean =
      processor != null && osName != null && libPath != null
  }

  object NativeCodeEntry {
    def parseFromEntry(entry: String): Option[NativeCodeEntry] = {
      val parsed = entry
        .split(";")
        .map(_.trim)
        .foldLeft(NativeCodeEntry(null, null, null)) { case (element, part) =>
          if (part.contains("=")) {
            val Array(k, v) = part.split("=", 2).map(_.trim)
            k match {
              case "processor" => element.copy(processor = v)
              case "osname"    => element.copy(osName = v)
              case _           => element
            }
          } else {
            element.copy(libPath = part)
          }
        }
      if (parsed.isValid) Some(parsed) else None
    }
  }
}
