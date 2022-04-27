
import sbt.internal.inc.CompilerArguments
import sbt.internal.inc.javac.JavacLogger
import sbt.io.IO
import sbt.util.Logger
import xsbti.{PathBasedFile, Reporter, VirtualFile, Logger => XLogger}
import xsbti.compile.{IncToolOptions, Output, JavaCompiler => XJavaCompiler}

import java.io.File
import java.nio.file.Path
import scala.sys.process.Process

object FrgaalJavaCompiler {

  /** Helper method to launch programs. */
  def launch(
              javaHome: Option[Path],
              compilerJar: Path,
              sources0: Seq[VirtualFile],
              options: Seq[String],
              output: Output,
              log: Logger,
              reporter: Reporter,
              source: String,
              target: String
            ): Boolean = {
    val (jArgs, nonJArgs) = options.partition(_.startsWith("-J"))
    val outputOption = CompilerArguments.outputOption(output)
    val sources = sources0 map {
      case x: PathBasedFile => x.toPath.toAbsolutePath.toString
    }
    val frgaalOptions = Seq("-source", source, "-target", target)
    val allArguments = outputOption ++ frgaalOptions ++ nonJArgs ++ sources

    //println("All params: " + options)
    //println("All Files: " + sources0)

    withArgumentFile(allArguments) { argsFile =>
      val forkArgs = (jArgs ++ Seq("--limit-modules", "java.base,jdk.zipfs", "-jar", compilerJar.toString)) :+ s"@${normalizeSlash(argsFile.getAbsolutePath)}"
      val exe = getJavaExecutable(javaHome, "java")
      val cwd = new File(new File(".").getAbsolutePath).getCanonicalFile
      val javacLogger = new JavacLogger(log, reporter, cwd)
      var exitCode = -1
      try {
        exitCode = Process(exe +: forkArgs, cwd) ! javacLogger
      } finally {
        javacLogger.flush("frgaal", exitCode)
      }
      // We return true or false, depending on success.
      exitCode == 0
    }
  }

  /**
    * Helper method to create an argument file that we pass to Javac.  Gets over the windows
    * command line length limitation.
    * @param args The string arguments to pass to Javac.
    * @param f  A function which is passed the arg file.
    * @tparam T The return type.
    * @return  The result of using the argument file.
    */
  def withArgumentFile[T](args: Seq[String])(f: File => T): T = {
    import IO.{ Newline, withTemporaryDirectory, write }
    withTemporaryDirectory { tmp =>
      val argFile = new File(tmp, "argfile")
      write(argFile, args.map(escapeSpaces).mkString(Newline))
      f(argFile)
    }
  }
  // javac's argument file seems to allow naive space escaping with quotes.  escaping a quote with a backslash does not work
  private def escapeSpaces(s: String): String = '\"' + normalizeSlash(s) + '\"'
  private def normalizeSlash(s: String) = s.replace(File.separatorChar, '/')

  /** create the executable name for java */
  def getJavaExecutable(javaHome: Option[Path], name: String): String =
    javaHome match {
      case None => name
      case Some(jh) =>
        jh.resolve("bin").resolve(name).toAbsolutePath.toString
    }
}

/** An implementation of compiling java which forks a Javac instance. */
final class FrgaalJavaCompiler(javaHome: Option[Path], compilerPath: Path, source: String="11", target: String="11") extends XJavaCompiler {
  def run(
           sources: Array[VirtualFile],
           options: Array[String],
           output: Output,
           incToolOptions: IncToolOptions,
           reporter: Reporter,
           log: XLogger
         ): Boolean =
    FrgaalJavaCompiler.launch(javaHome, compilerPath, sources, options, output, log, reporter, source, target)
}