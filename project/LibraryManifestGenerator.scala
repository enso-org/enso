import sbt.*
import sbt.util.CacheStoreFactory

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.jdk.javaapi.CollectionConverters.asJava
import scala.sys.process.ProcessLogger

/** A helper for generating manifests for bundled libraries. */
object LibraryManifestGenerator {

  /** Represents a library that will be bundled with the engine and needs to
    * have its manifest generated.
    */
  case class BundledLibrary(name: String, version: String)

  /** Generates manifests for the provided libraries.
    *
    * It assumes that the engine-runner/assembly task is up to date (as it uses
    * its artifacts).
    *
    * @param javaOpts The java options to pass to the manifest generator.
    */
  def generateManifests(
    libraries: Seq[BundledLibrary],
    distributionRoot: File,
    log: Logger,
    javaOpts: Seq[String],
    cacheStoreFactory: CacheStoreFactory,
    env: Map[String, String] = Map.empty
  ): Unit =
    for (BundledLibrary(qualifiedName, version) <- libraries) {
      val (namespace, name) = qualifiedName.split('.') match {
        case Array(namespace, name) => (namespace, name)
        case _ =>
          throw new IllegalArgumentException(
            s"Invalid library name [$qualifiedName]."
          )
      }
      val projectPath =
        distributionRoot / "lib" / namespace / name / version

      val store =
        cacheStoreFactory.make(s"library-manifest-$namespace-$name-$version")
      val sources = (projectPath / "src").allPaths.get
      Tracked.diffInputs(store, FileInfo.hash)(sources.toSet) { diff =>
        def manifestExists = (projectPath / "manifest.yaml").exists()
        if (diff.modified.nonEmpty || !manifestExists) {
          log.info(s"Regenerating manifest for [$projectPath].")
          runGenerator(projectPath, javaOpts, log, env)
        } else {
          log.debug(s"[$projectPath] manifest is up to date.")
        }
      }
    }

  private def runGenerator(
    projectPath: File,
    javaOpts: Seq[String],
    log: Logger,
    env: Map[String, String] = Map.empty
  ): Unit = {
    val canonicalPath = projectPath.getCanonicalFile
    val javaCommand   = javaExecutable()
    val command = Seq(
      javaCommand
    ) ++ javaOpts ++ Seq(
      "--update-manifest",
      "--in-project",
      canonicalPath.toString
    )

    val allEnv = Map(
      "ENSO_EDITION_PATH" -> file("distribution/editions").getCanonicalPath
    ) ++ env
    val commandText = command.mkString(" ")
    log.debug(s"Running [$commandText].")
    val procBldr = new java.lang.ProcessBuilder(asJava(command))
    procBldr.directory(canonicalPath.getParentFile)
    allEnv.foreach { case (key, value) =>
      procBldr.environment().put(key, value)
    }

    val processOutLines: ListBuffer[String] = ListBuffer()
    val captureOut = ProcessLogger(
      fout = (s: String) => {
        processOutLines.append("[stdout] " + s)
      },
      ferr = (s: String) => {
        processOutLines.append("[stderr] " + s)
      }
    )
    val exitCode = sys.process.Process(procBldr).!(captureOut)
    if (exitCode != 0) {
      val message = s"Command [$commandText] has failed with code $exitCode:"
      log.error(message)
      log.error(processOutLines.mkString("\n"))
      throw new RuntimeException(message)
    }
  }

  private def javaExecutable(): String = {
    val jHome = System.getProperty("java.home")
    if (jHome != null) {
      if (Platform.isWindows) {
        jHome + File.separator + "bin" + File.separator + "java.exe"
      } else {
        jHome + File.separator + "bin" + File.separator + "java"
      }
    } else {
      ProcessHandle.current().info().command().asScala.getOrElse("java")
    }
  }

}
