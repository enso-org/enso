import java.io.IOException

import sbt.Def

import scala.sys.process._

object StdBits {
  def preparePackage =
    Def.task {
      val cmd = Seq("mvn", "package", "-f", "std-bits")
      val exitCode =
        try {
          if (Platform.isWindows) {
            (Seq("cmd", "/c") ++ cmd).!
          } else {
            cmd.!
          }
        } catch {
          case e @ (_: RuntimeException | _: IOException) =>
            throw new RuntimeException(
              "Cannot run `mvn`, " +
              "make sure that it is installed and present on your PATH.",
              e
            )
        }

      if (exitCode != 0) {
        throw new RuntimeException("std-bits build failed.")
      }
    }

}
