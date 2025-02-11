import sbt.*
import sbt.Keys.TaskStreams

import scala.sys.process.*

object Ydoc {

  private val corepackCommand =
    if (Platform.isWindows) "corepack.cmd" else "corepack"

  private val pnpmCommand =
    s"$corepackCommand pnpm"

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
    val store = streams.cacheStoreFactory.make("ydoc-server-npm-compile-cache")
    val generator = Tracked.inputChanged[Seq[File], Seq[File]](store) {
      case (changed, _) =>
        val resourceYdocServerJs =
          ydocServerResourceManaged / "org" / "enso" / "ydoc" / "server" / "ydoc.cjs"

        if (changed) {
          runCommand(s"$corepackCommand --version", streams)
          runCommand(s"$pnpmCommand --version", streams)

          val command  = s"$pnpmCommand build:ydoc-server-polyglot"
          val exitCode = runCommand(command, streams)
          if (exitCode != 0) {
            throw new CommandFailed(command, exitCode)
          }

          val generatedYdocServerJs =
            base / "app" / "ydoc-server-polyglot" / "dist" / "main.cjs"
          IO.copyFile(generatedYdocServerJs, resourceYdocServerJs)
        }

        Seq(resourceYdocServerJs)
    }

    val sourceFiles: PathFinder =
      (base / "app") ** ("*.js" | "*.ts" | "*.json" | "*.rs" | "*.toml")
    val nodeModulesFiles =
      (base / "app") ** "node_modules" ** "*"
    val ideDesktopFiles =
      (base / "app") ** "ide-desktop" ** "*"

    val inputFiles = sourceFiles --- nodeModulesFiles --- ideDesktopFiles

    generator(inputFiles.get())
  }

  private def runNpmInstallCached(base: File, streams: TaskStreams): Unit = {
    val store = streams.cacheStoreFactory.make("ydoc-server-npm-install-cache")
    val generator = Tracked.inputChanged[File, Unit](store) {
      case (changed, _) =>
        val nodeModules = base / "node_modules"
        if (changed || !nodeModules.isDirectory) {
          runCommand(s"$corepackCommand --version", streams)
          runCommand(s"$pnpmCommand --version", streams)

          val command  = s"$pnpmCommand i --frozen-lockfile"
          val exitCode = runCommand(command, streams)
          if (exitCode != 0) {
            throw new CommandFailed(command, exitCode)
          }
        }
    }

    val inputFile = base / "pnpm-lock.json"

    generator(inputFile)
  }

  /** Run command printing the output to the log stream.
    *
    * @param command the command to run
    * @param streams the build streams
    * @return exit code
    */
  private def runCommand(command: String, streams: TaskStreams): Int = {
    streams.log.info(command)
    command ! streams.log
  }

  final private class CommandFailed(command: String, exitCode: Int)
      extends FeedbackProvidedException {

    override def toString: String = {
      s"Command [$command] failed with exit code [$exitCode]"
    }
  }
}
