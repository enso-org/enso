import java.io.IOException

import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object GenerateAST {

  val rustVersion = settingKey[String]("rustc version used in the project")
  private val cargoCmd = "cargo"

  lazy val task = Def.task {
    val log = state.value.log

    verifyRustVersion(rustVersion.value) match {
      case Left(explanation) =>
        log.error(explanation)
        throw new RuntimeException("Rust version check failed.")
      case Right(_) =>
    }

    Seq(generateAST(new File("lib/scala/ast/ast.scala"), log))
  }

  /**
    * Runs cargo to ensure that it is properly installed and checks if the
    * reported rust version is consistent with expectations.
    *
    * @param expectedVersion rust version that is expected to be installed,
    *                        should be based on project settings
    * @return either an error message explaining what is wrong with the rust
    *         version or Unit meaning it is correct
    */
  def verifyRustVersion(expectedVersion: String): Either[String, Unit] = {
    val cmd = f"$cargoCmd --version"
    try {
      val versionStr = cmd.!!.trim.split(' ')(1)

      if (versionStr == expectedVersion) Right(()) else {
        Left(
          s"Rust version mismatch. $expectedVersion is expected, " +
          s"but it seems $versionStr is installed."
        )
      }
    } catch {
      case _ @(_: RuntimeException | _: IOException) =>
        Left("Rust version check failed. Make sure cargo is in your PATH.")
    }
  }


  /**
    * Generates the Scala AST in specified file. All encountered errors are
    * reported in stderr and rise a runtime exception.
    *
    * @param out the file where the generated AST is going to be placed
    * @return the file containing generated AST
    */
  private def generateAST(out: File, log: ManagedLogger): File = {
    val command = s"$cargoCmd run -p ast -- --generate-scala-ast $out"

    log.info(s"Generating AST with the command: $command:")

    try {command.!!} catch {
      case ex: RuntimeException =>
        log.error(s"Generation of the Scala AST failed.")
        throw ex
    }
    out
  }
}
