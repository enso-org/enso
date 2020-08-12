import java.io.IOException

import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

import scala.sys.process._

object GenerateAST {

  val rustVersion = settingKey[String]("rustc version used in the project")
  private val cargoCmd = "cargo"

  lazy val task = Def.task {
    val log    = state.value.log
    val lib    = baseDirectory.value.getParentFile.getParentFile / "lib"
    val source = lib / "rust/ast/src/ast.rs"
    val output = sourceManaged.value / "main/org/enso/ast/Ast.scala"
    val cache  = streams.value.cacheStoreFactory.make("ast_source")

    if (!EnvironmentCheck.rustVersionOk(rustVersion.value, log))
      throw new RuntimeException("Rust version mismatch!")

    Tracked.diffInputs(cache, FileInfo.lastModified)(Set(source))
    { source: ChangeReport[File] =>
      if (source.modified.nonEmpty) {
          output.getParentFile.mkdirs
          generateAST(output, log)
      }
    }

    Seq(output)
  }

  /**
    * Generates the Scala AST in the specified file. All errors are reported in 
    * stderr and raise a runtime exception.
    *
    * @param out the file where the generated AST is going to be placed
    */
  private def generateAST(out: File, log: ManagedLogger): Unit = {
    val command = s"$cargoCmd run -p ast -- --generate-scala-ast $out"

    log.info(s"Generating AST with the command: $command:")

    try {command.!!} catch {
      case ex: RuntimeException =>
        log.error(s"Generation of the Scala AST failed.")
        throw ex
    }
  }
}
