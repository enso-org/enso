import sbt.io.IO

import java.io.File
import java.nio.file.{Path, Paths}
import scala.collection.immutable.Seq

/** Building small JDK distributions with `jlink` command
  */
object SmallJDK {

  private val NI_BUILDER_MODULES = Seq(
    "org.graalvm.nativeimage.builder",
    "org.graalvm.nativeimage.driver",
    "org.graalvm.nativeimage.librarysupport",
    "org.graalvm.nativeimage.objectfile",
    "org.graalvm.nativeimage.pointsto"
  )

  private val NI_BASE_MODULES = Seq(
    "org.graalvm.nativeimage",
    "org.graalvm.nativeimage.base",
    "com.oracle.graal.graal_enterprise",
    "com.oracle.svm.svm_enterprise"
  )

  private val JDK_MODULES = Seq(
    "java.naming",
    "java.net.http",
    "java.rmi",
    "jdk.attach",
    "jdk.charsets",
    "jdk.crypto.ec",
    "jdk.httpserver",
    "jdk.localedata"
  )

  private val ADDITIONAL_NI_BUILD_MODS = Seq(
    "java.prefs"
  )

  private val DEBUG_MODULES = Seq(
    "jdk.jdwp.agent"
  )

  private val PYTHON_MODULES = Seq(
    "java.naming",
    "jdk.security.auth"
  )

  /** Builds a small JDK appropriate for building native image.
    * @param targetJdkDirectory Target directory. If non empty, will be deleted.
    */
  def buildSmallJDKForNativeImage(
    targetJdkDirectory: File
  ): Unit = {
    if (targetJdkDirectory.exists()) {
      IO.delete(targetJdkDirectory)
    }
    val niModules     = (NI_BASE_MODULES ++ NI_BUILDER_MODULES).mkString(",")
    val jdkModules    = (JDK_MODULES ++ ADDITIONAL_NI_BUILD_MODS).mkString(",")
    val debugModules  = DEBUG_MODULES.mkString(",")
    val pythonModules = PYTHON_MODULES.mkString(",")

    val mp = modulePath()
      .map(_.toAbsolutePath.toString)
      .mkString(File.pathSeparator)

    val jlinkArgs = Seq(
      "--module-path",
      mp,
      "--output",
      targetJdkDirectory.toString,
      "--add-modules",
      s"$niModules,$jdkModules,$debugModules,$pythonModules"
    )
    runJlink(jlinkArgs)
    copyLibDirs(targetJdkDirectory)
    assert(
      targetJdkDirectory.exists(),
      "Directory of small JDK " + targetJdkDirectory + " is not present"
    )
  }

  /** Builds a small JDK with `jlink` appropriate for running
    * Enso in `--jvm` mode.
    * @param targetJdkDirectory Target directory. If not empty,
    *                          will be deleted.
    */
  def buildSmallJDKForRelease(
    targetJdkDirectory: File
  ): Unit = {
    if (targetJdkDirectory.exists()) {
      IO.delete(targetJdkDirectory)
    }
    val mp = modulePath()
      .map(_.toAbsolutePath.toString)
      .mkString(File.pathSeparator)
    val modules =
      JDK_MODULES ++ NI_BASE_MODULES ++ DEBUG_MODULES ++ PYTHON_MODULES
    val jlinkArgs = Seq(
      "--no-header-files",
      "--no-man-pages",
      "--module-path",
      mp,
      "--output",
      targetJdkDirectory.toString,
      "--add-modules",
      modules.mkString(",")
    )
    runJlink(jlinkArgs)
    copyLibDirs(targetJdkDirectory)
    assert(
      targetJdkDirectory.exists(),
      "Directory of small JDK " + targetJdkDirectory +
      " was not created."
    )
  }

  private def runJlink(
    args: Seq[String]
  ): Unit = {
    val exitCode = scala.sys.process.Process(jlink().toString, args).!
    if (exitCode != 0) {
      throw new RuntimeException(
        s"Failed to execute ${jlink()} ${args.mkString(" ")} - exit code: $exitCode"
      )
    }
  }

  private def copyLibDirs(smallJdkDirectory: File): Unit = {
    libDirs().foreach(libDir =>
      IO.copyDirectory(
        libDir.toFile,
        smallJdkDirectory.toPath
          .resolve("lib")
          .resolve(libDir.toFile.getName)
          .toFile
      )
    )
  }

  private def javaHome(): Path = {
    val prop = System.getProperty("java.home")
    if (prop == null) {
      throw new RuntimeException("Missing java.home prop")
    } else {
      Path.of(prop)
    }
  }

  private def jlink(): Path = {
    javaHome().resolve("bin").resolve("jlink")
  }

  private def modulePath(): List[Path] = {
    val moduleJars = List(
      "lib/svm/bin/../../graalvm/svm-driver.jar",
      "lib/svm/bin/../builder/native-image-base.jar",
      "lib/svm/bin/../builder/espresso-svm.jar",
      "lib/svm/bin/../builder/objectfile.jar",
      "lib/svm/bin/../builder/pointsto.jar",
      "lib/svm/bin/../builder/reporter.jar",
      "lib/svm/bin/../builder/svm-enterprise.jar",
      "lib/svm/bin/../builder/svm.jar",
      "lib/svm/bin/../builder/svm-configure.jar",
      "lib/svm/bin/../builder/svm-capnproto-runtime.jar",
      "lib/svm/bin/../builder/svm-foreign.jar",
      "lib/svm/bin/../library-support.jar"
    )
    moduleJars.map { jar =>
      javaHome().resolve(jar)
    }
  }

  private def libDirs(): List[Path] = {
    val targetLibDirs = List(
      "graalvm",
      "svm",
      "static",
      "truffle"
    )
    targetLibDirs.map { d =>
      javaHome().resolve("lib").resolve(d)
    }
  }
}
