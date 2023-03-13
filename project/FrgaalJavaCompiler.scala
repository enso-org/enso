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
import scala.util.Using
import java.io.FileWriter

object FrgaalJavaCompiler {
  private val ENSO_SOURCES = ".enso-sources"

  val frgaal = "org.frgaal" % "compiler" % "19.0.0" % "provided"

  def compilers(
    classpath: sbt.Keys.Classpath,
    sbtCompilers: xsbti.compile.Compilers,
    javaVersion: String
  ) = {
    // Enable Java 11+ features by invoking Frgaal instead of regular javac
    val javaHome = Option(System.getProperty("java.home")).map(Paths.get(_))

    // Locate frgaal compiler jar from the list of dependencies
    val frgaalModule = FrgaalJavaCompiler.frgaal
    val frgaalCheck = (module: ModuleID) =>
      module.organization == frgaalModule.organization &&
      module.name == frgaalModule.name &&
      module.revision == frgaalModule.revision
    val frgaalOnClasspath =
      classpath
        .find(f =>
          f.metadata
            .get(AttributeKey[ModuleID]("moduleID"))
            .map(frgaalCheck)
            .getOrElse(false)
        )
        .map(_.data.toPath)
    if (frgaalOnClasspath.isEmpty) {
      throw new RuntimeException("Failed to resolve Frgaal compiler. Aborting!")
    }
    val frgaalJavac = new FrgaalJavaCompiler(
      javaHome,
      frgaalOnClasspath.get,
      target = javaVersion
    )
    val javaTools = sbt.internal.inc.javac
      .JavaTools(frgaalJavac, sbtCompilers.javaTools.javadoc())
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
    val outputOption      = CompilerArguments.outputOption(output)
    val sources = sources0 map { case x: PathBasedFile =>
      x.toPath.toAbsolutePath.toString
    }

    def asPath(a: Any): Path = a match {
      case p: PathBasedFile => p.toPath
      case p: Path          => p
    }

    def asCommon(a: Any, b: Any): Path = {
      var ap = asPath(a)
      val bp = asPath(b)

      var i = 0
      while (
        i < Math.min(ap.getNameCount(), bp.getNameCount()) && ap.getName(
          i
        ) == bp.getName(i)
      ) {
        i += 1;
      }

      while (ap.getNameCount() > i) {
        ap = ap.getParent()
      }
      ap
    }

    val out    = output.getSingleOutputAsPath().get()
    val shared = sources0.fold(out)(asCommon).asInstanceOf[Path]

    // searching for $shared/src/main/java or
    // $shared/src/test/java or
    // $shared/src/bench/java or etc.
    def findUnder(depth: Int, dir: Path): Path = {
      var d = dir
      while (d.getNameCount() > depth) {
        val threeUp  = d.subpath(0, d.getNameCount() - depth)
        val relShare = shared.subpath(0, shared.getNameCount())
        if (relShare.equals(threeUp)) {
          return d
        } else {
          d = d.getParent()
        }
      }
      throw new IllegalArgumentException(
        "Cannot findUnder for " + dir + " and " + shared +
        "\nout: " + out + "\nsources: " + sources
      )
    }
    def checkTarget(x: Any) = {
      val p = asPath(x)
      val namesCheck =
        for (i <- 0 until p.getNameCount)
          yield "target".equals(p.getName(i).toString()) || p
            .getName(i)
            .toString()
            .endsWith("-windows") || p.getName(i).toString().endsWith("-unix")
      val inATargetDir = namesCheck.exists(x => x)
      inATargetDir
    }

    val (withTarget, noTarget) = sources0.partition(checkTarget)
    val in = if (noTarget.isEmpty) {
      None
    } else {
      Some(
        findUnder(
          3,
          noTarget.tail.fold(asPath(noTarget.head))(asCommon).asInstanceOf[Path]
        )
      )
    }
    val generated = if (withTarget.isEmpty) {
      None
    } else {
      Some(
        findUnder(
          4,
          withTarget.tail
            .fold(asPath(withTarget.head))(asCommon)
            .asInstanceOf[Path]
        )
      )
    }

    if (shared.toFile().exists()) {
      val ensoMarker = new File(shared.toFile(), ENSO_SOURCES)
      val ensoConfig = new File(
        shared.toFile(),
        ENSO_SOURCES + "-" + out.getFileName().toString()
      )
      val ensoProperties = new java.util.Properties()

      def storeArray(name: String, values: Seq[String]) = {
        values.zipWithIndex.foreach { case (value, idx) =>
          ensoProperties.setProperty(s"$name.$idx", value)
        }
      }
      if (in.isDefined) {
        ensoProperties.setProperty("input", in.get.toString())
      }
      if (generated.isDefined) {
        ensoProperties.setProperty("generated", generated.get.toString())
      }
      ensoProperties.setProperty("output", out.toString())
      storeArray("options", options)
      source.foreach(v => ensoProperties.setProperty("source", v))
      ensoProperties.setProperty("target", target)
      javaHome.foreach(v =>
        ensoProperties.setProperty("java.home", v.toString())
      )

      Using(new FileWriter(ensoConfig)) { w =>
        ensoProperties.store(w, "# Enso compiler configuration")
      }
      Using(new FileWriter(ensoMarker)) { _ => }
    } else {
      throw new IllegalStateException(
        "Cannot write Enso source options to " + shared + " values:\n" +
        "options: " + options + " sources0: " + sources + " output: " + output
      )
    }

    val frgaalOptions: Seq[String] =
      source.map(v => Seq("-source", v)).getOrElse(Seq()) ++ Seq(
        "-target",
        target
      )
    val allArguments = outputOption ++ frgaalOptions ++ nonJArgs ++ sources

    withArgumentFile(allArguments) { argsFile =>
      // Need to disable standard compiler tools that come with used jdk and replace them
      // with the ones provided with Frgaal.
      val forkArgs = (jArgs ++ Seq(
        "--limit-modules",
        "java.base,jdk.zipfs,jdk.internal.vm.compiler.management",
        "-jar",
        compilerJar.toString
      )) :+
        s"@${normalizeSlash(argsFile.getAbsolutePath)}"
      val exe         = getJavaExecutable(javaHome, "java")
      val cwd         = new File(new File(".").getAbsolutePath).getCanonicalFile
      val javacLogger = new JavacLogger(log, reporter, cwd)
      var exitCode    = -1
      try {
        exitCode = Process(exe +: forkArgs, cwd) ! javacLogger
      } finally {
        javacLogger.flush("frgaal", exitCode)
      }
      // We return true or false, depending on success.
      exitCode == 0
    }
  }

  /** Helper method to create an argument file that we pass to Javac.  Gets over the windows
    * command line length limitation.
    * @param args The string arguments to pass to Javac.
    * @param f  A function which is passed the arg file.
    * @tparam T The return type.
    * @return  The result of using the argument file.
    */
  def withArgumentFile[T](args: Seq[String])(f: File => T): T = {
    import IO.{withTemporaryDirectory, write, Newline}
    withTemporaryDirectory { tmp =>
      val argFile = new File(tmp, "argfile")
      write(argFile, args.map(escapeSpaces).mkString(Newline))
      f(argFile)
    }
  }
  // javac's argument file seems to allow naive space escaping with quotes.  escaping a quote with a backslash does not work
  private def escapeSpaces(s: String): String = '\"' + normalizeSlash(s) + '\"'
  private def normalizeSlash(s: String)       = s.replace(File.separatorChar, '/')

  /** create the executable name for java */
  def getJavaExecutable(javaHome: Option[Path], name: String): String =
    javaHome match {
      case None => name
      case Some(jh) =>
        jh.resolve("bin").resolve(name).toAbsolutePath.toString
    }
}

/** An implementation of compiling java which forks Frgaal instance. */
final class FrgaalJavaCompiler(
  javaHome: Option[Path],
  compilerPath: Path,
  target: String,
  source: Option[String] = None
) extends XJavaCompiler {
  def run(
    sources: Array[VirtualFile],
    options: Array[String],
    output: Output,
    incToolOptions: IncToolOptions,
    reporter: Reporter,
    log: XLogger
  ): Boolean =
    FrgaalJavaCompiler.launch(
      javaHome,
      compilerPath,
      sources,
      options,
      output,
      log,
      reporter,
      source,
      target
    )
}
