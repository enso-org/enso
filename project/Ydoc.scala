import sbt.*
import sbt.Keys.TaskStreams

import scala.sys.process.*

object Ydoc {

  private val npmCommand = if (Platform.isWindows) "npm.cmd" else "npm"

  /** Generates the bundled JS source of the Ydoc server.
    *
    * @param buildBase the path to the base directory of this build
    * @param ydocServerBase the path to the base directory of the ydoc-server project
    * @param streams the build streams
    * @return the list of generated files
    */
  def generateJsBundle(
    buildBase: File,
    ydocServerBase: File,
    streams: TaskStreams
  ): Seq[File] = {
    val ydocServerSrc = (buildBase / "app" / "gui2" / "ydoc-server") ** "*.ts"
    val sharedSrc     = (buildBase / "app" / "gui2" / "shared") ** "*.ts"
    val buildCfg      = (buildBase / "app" / "gui2") * ("*.ts" || "*.json")
    val inputFiles    = ydocServerSrc +++ sharedSrc +++ buildCfg

    generateJsBundleCached(buildBase, ydocServerBase, streams, inputFiles.get)
  }

  /** Cached JS bundle generator. Invokes the `npm` build only the input files have been changed.
    *
    * @param buildBase the path to the base directory of this build
    * @param ydocServerBase the path to the base directory of the ydoc-server project
    * @param streams the build streams
    * @param inputFiles the list of input files required for generating JS bundle
    * @return the list of generated files
    */
  private def generateJsBundleCached(
    buildBase: File,
    ydocServerBase: File,
    streams: TaskStreams,
    inputFiles: Seq[File]
  ): Seq[File] = {
    val store = streams.cacheStoreFactory.make("ydoc-server-cache")
    val generator = Tracked.inputChanged[Seq[File], Seq[File]](store) {
      case (changed, _) =>
        if (changed) {
          s"$npmCommand --workspace=enso-gui2 run build-ydoc-server-polyglot" ! streams.log
        }
        val generatedFiles =
          (ydocServerBase / "target" / "ydoc-server-bundle" / "assets") * "*.js"
        generatedFiles.get
    }

    generator(inputFiles)
  }
}
