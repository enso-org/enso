import sbt.io.IO

import java.io.File
import java.nio.file.Paths
import scala.collection.immutable.Seq

/**
 * Building small JDK distributions with `jlink` command
 */
object SmallJDK {
  /**
   * Builds a small JDK appropriate for building native image.
   * @param smallJdkDirectory Target directory. If non empty, will be deleted.
   */
  def buildSmallJDKForNativeImage(
    smallJdkDirectory: File
  ): Unit = {
    if (smallJdkDirectory.exists()) {
      IO.delete(smallJdkDirectory)
    }
    val NI_MODULES =
      "org.graalvm.nativeimage,org.graalvm.nativeimage.builder,org.graalvm.nativeimage.base,org.graalvm.nativeimage.driver,org.graalvm.nativeimage.librarysupport,org.graalvm.nativeimage.objectfile,org.graalvm.nativeimage.pointsto,com.oracle.graal.graal_enterprise,com.oracle.svm.svm_enterprise"
    val JDK_MODULES =
      "java.naming,java.net.http,jdk.charsets,jdk.crypto.ec,jdk.localedata,jdk.httpserver,java.rmi"
    val DEBUG_MODULES  = "jdk.jdwp.agent"
    val PYTHON_MODULES = "jdk.security.auth,java.naming"

    val javaHome = Option(System.getProperty("java.home")).map(Paths.get(_))
    val (jlink, modules, libDirs) = javaHome match {
      case None =>
        throw new RuntimeException("Missing java.home variable")
      case Some(jh) =>
        val exec = jh.resolve("bin").resolve("jlink")
        val moduleJars = List(
          "lib/svm/bin/../../graalvm/svm-driver.jar",
          "lib/svm/bin/../builder/native-image-base.jar",
          "lib/svm/bin/../builder/objectfile.jar",
          "lib/svm/bin/../builder/pointsto.jar",
          "lib/svm/bin/../builder/svm-enterprise.jar",
          "lib/svm/bin/../builder/svm.jar",
          "lib/svm/bin/../library-support.jar"
        )
        val targetLibDirs = List("graalvm", "svm", "static", "truffle")
        (
          exec,
          moduleJars.map(jar => jh.resolve(jar).toString),
          targetLibDirs.map(d => jh.resolve("lib").resolve(d))
        )
    }

    val jlinkArgs = Seq(
      "--module-path",
      modules.mkString(File.pathSeparator),
      "--output",
      smallJdkDirectory.toString(),
      "--add-modules",
      s"$NI_MODULES,$JDK_MODULES,$DEBUG_MODULES,$PYTHON_MODULES"
    )
    val exitCode = scala.sys.process.Process(jlink.toString(), jlinkArgs).!
    if (exitCode != 0) {
      throw new RuntimeException(
        s"Failed to execute $jlink ${jlinkArgs.mkString(" ")} - exit code: $exitCode"
      )
    }
    libDirs.foreach(libDir =>
      IO.copyDirectory(
        libDir.toFile,
        smallJdkDirectory.toPath
          .resolve("lib")
          .resolve(libDir.toFile.getName)
          .toFile
      )
    )
    assert(
      smallJdkDirectory.exists(),
      "Directory of small JDK " + smallJdkDirectory + " is not present"
    )
  }
}
