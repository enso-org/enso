import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger

object GenerateAST {

  lazy val task = Def.task {
    val log    = state.value.log
    val lib    = baseDirectory.value.getParentFile.getParentFile
    val source = lib / "rust/ast/src/ast.rs"
    val output = sourceManaged.value / "main/org/enso/ast/Ast.scala"
    val cache  = streams.value.cacheStoreFactory.make("ast_source")

    Tracked.diffInputs(cache, FileInfo.lastModified)(Set(source)) {
      source: ChangeReport[File] =>
        val rustVersion = Cargo.rustVersion.value
        if (source.modified.nonEmpty) {
          output.getParentFile.mkdirs
          generateAST(rustVersion, output, log)
        }
    }

    Seq(output)
  }

  /** Generates the Scala AST in the specified file. All errors are reported in
    * stderr and raise a runtime exception.
    *
    * @param out the file where the generated AST is going to be placed
    */
  def generateAST(rustVersion: String, out: File, log: ManagedLogger): Unit = {
    val args = s"run -p ast -- --generate-scala-ast $out"

    log.info(s"Generating Scala AST from Rust definitions.")

    try Cargo.run(args, rustVersion, log)
    catch {
      case ex: RuntimeException =>
        log.error(s"Generation of the Scala AST failed.")
        throw ex
    }
  }
}
