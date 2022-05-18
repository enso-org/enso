/*
 * The FrgaalJavaCompiler adapts ForkedJava from Zinc
 * to invoke Frgaal compiler instead of javac.
 *
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 */

import sbt._
import sbt.internal.inc.CompilerArguments
import sbt.internal.inc.javac.JavacLogger
import sbt.io.IO
import sbt.util.Logger
import xsbti.{PathBasedFile, Reporter, VirtualFile, Logger => XLogger}
import xsbti.compile.{IncToolOptions, Output, JavaCompiler => XJavaCompiler}

import java.io.File
import java.nio.file.{Path, Paths}
import scala.sys.process.Process

object FrgaalJavaCompiler {

  val frgaal = "org.frgaal" % "compiler" % "18.0.0" % "provided"

  def compilers(classpath: sbt.Keys.Classpath, sbtCompilers: xsbti.compile.Compilers, javaVersion: String) = {
    // Enable Java 11+ features by invoking Frgaal instead of regular javac
    val javaHome = Option(System.getProperty("java.home")).map(Paths.get(_))

    // Locate frgaal compiler jar from the list of dependencies
    val frgaalModule = FrgaalJavaCompiler.frgaal
    val frgaalCheck = (module: ModuleID) =>
      module.organization == frgaalModule.organization &&
        module.name == frgaalModule.name &&
        module.revision == frgaalModule.revision
    val frgaalOnClasspath =
      classpath.find(f => f.metadata.get(AttributeKey[ModuleID]("moduleID")).map(frgaalCheck).getOrElse(false))
        .map(_.data.toPath)
    if (frgaalOnClasspath.isEmpty) {
      throw new RuntimeException("Failed to resolve Frgaal compiler. Aborting!")
    }
    val frgaalJavac = new FrgaalJavaCompiler(javaHome, frgaalOnClasspath.get, target = javaVersion)
    val javaTools = sbt.internal.inc.javac.JavaTools(frgaalJavac, sbtCompilers.javaTools.javadoc())
    xsbti.compile.Compilers.of(sbtCompilers.scalac, javaTools)
  }

  /** Helper method to launch programs. */
  def launch(
              javaHome: Option[Path],
              compilerJar: Path,
              sources0: Seq[VirtualFile],
              options: Seq[String],
              output: Output,
              log: Logger,
              reporter: Reporter,
              source: Option[String],
              target: String
            ): Boolean = {
    val (jArgs, nonJArgs) = options.partition(_.startsWith("-J"))
    val outputOption = CompilerArguments.outputOption(output)
    val sources = sources0 map {
      case x: PathBasedFile => x.toPath.toAbsolutePath.toString
    }
    val frgaalOptions: Seq[String] = source.map(v => Seq("-source", v)).getOrElse(Seq()) ++ Seq("-target", target)
    val allArguments = outputOption ++ frgaalOptions ++ nonJArgs ++ sources

    withArgumentFile(allArguments) { argsFile =>
      // Need to disable standard compiler tools that come with used jdk and replace them
      // with the ones provided with Frgaal.
      val forkArgs = (jArgs ++ Seq("--limit-modules", "java.base,jdk.zipfs", "-jar", compilerJar.toString)) :+
         s"@${normalizeSlash(argsFile.getAbsolutePath)}"
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

/** An implementation of compiling java which forks Frgaal instance. */
final class FrgaalJavaCompiler(javaHome: Option[Path], compilerPath: Path, target: String, source: Option[String] = None) extends XJavaCompiler {
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
