import sbt.*
import sbt.Keys.TaskStreams

import scala.sys.process.*

object Ydoc {

  private val pnpmCommand =
    if (Platform.isWindows) "corepack.cmd pnpm" else "corepack pnpm"

  /** Generates the bundled JS source of the Ydoc server.
    *
    * @param base the path to the base directory of this build
    * @param ydocServerBase the path to the base directory of the ydoc-server project
    * @param ydocServerResourceManaged the paht to the managed resources directory
    * @param streams the build streams
    * @return the list of generated files
    */
  def generateJsBundle(
    base: File,
    ydocServerBase: File,
    ydocServerResourceManaged: File,
    streams: TaskStreams
  ): Seq[File] = {
    runNpmInstallCached(base, streams)

    generateJsBundleCached(
      base,
      ydocServerBase,
      ydocServerResourceManaged,
      streams
    )
  }

  /** Cached JS bundle generator. Invokes the `npm` build only the input files have been changed.
    *
    * @param base the path to the base directory of this build
    * @param ydocServerBase the path to the base directory of the ydoc-server project
    * @param ydocServerResourceManaged the path the managed resources directory
    * @param streams the build streams
    * @return the list of generated files
    */
  private def generateJsBundleCached(
    base: File,
    ydocServerBase: File,
    ydocServerResourceManaged: File,
    streams: TaskStreams
  ): Seq[File] = {
    val store = streams.cacheStoreFactory.make("ydoc-server-cache")
    val generator = Tracked.inputChanged[Seq[File], Seq[File]](store) {
      case (changed, _) =>
        val resourceYdocServerJs =
          ydocServerResourceManaged / "org" / "enso" / "ydoc" / "ydoc.cjs"

        if (changed) {
          val command = s"$pnpmCommand -r compile"
          streams.log.info(command)
          command ! streams.log
          val generatedYdocServerJs =
            base / "app" / "ydoc-server-polyglot" / "dist" / "main.cjs"
          IO.copyFile(generatedYdocServerJs, resourceYdocServerJs)
        }

        Seq(resourceYdocServerJs)
    }

    val ydocServerPolyglot = base / "app" / "ydoc-server-polyglot"
    val ydocServerPolyglotSrc =
      ydocServerPolyglot * ("*.mjs" | "*.json") +++ (ydocServerPolyglot / "src") ** "*.ts"
    val ydocShared = base / "app" / "ydoc-shared"
    val ydocSharedSrc =
      ydocShared * ("*.ts" || "*.json") +++
      (ydocShared / "src") ** "*.ts" +++
      (ydocShared / "parser-codegen") ** "*.ts"
      // TODO: ????
    val sharedSrc  = (base / "app" / "gui2" / "shared") ** "*.ts"
    val inputFiles = ydocServerPolyglotSrc +++ sharedSrc

    generator(inputFiles.get)
  }

  private def runNpmInstallCached(base: File, streams: TaskStreams): Unit = {
    val store = streams.cacheStoreFactory.make("ydoc-server-npm-install-cache")
    val generator = Tracked.inputChanged[File, Unit](store) {
      case (changed, _) =>
        val nodeModules = base / "node_modules"
        if (changed || !nodeModules.isDirectory) {
          val command = s"$pnpmCommand i --frozen-lockfile"
          streams.log.info(command)
          command ! streams.log
        }
    }

    val inputFile = base / "package-lock.json"

    generator(inputFile)
  }
}
