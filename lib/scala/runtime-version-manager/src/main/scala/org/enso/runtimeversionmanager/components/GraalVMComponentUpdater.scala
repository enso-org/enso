package org.enso.runtimeversionmanager.components

import java.nio.file.Path
import com.typesafe.scalalogging.Logger

import scala.sys.process._
import scala.util.{Failure, Success, Try}

/** Module that manages components of the GraalVM distribution.
  *
  * @param runtime the GraalVM runtime
  */
class GraalVMComponentUpdater(runtime: GraalRuntime)
    extends RuntimeComponentUpdater {

  import GraalVMComponentUpdater._

  private val logger = Logger[GraalVMComponentUpdater]
  private val gu     = runtime.findExecutable("gu")

  /** Path to the GraalVM's updater.
    *
    * @return path that will be executed to call the updater
    */
  protected def updaterExec: Path = gu

  /** List the installed GraalVM components.
    *
    * @return the list of installed GraalVM components
    */
  override def list(): Try[Seq[GraalVMComponent]] = {
    val command = Seq("list", "-v")

    logger.trace("{} {}", gu, Properties(gu))
    logger.debug(
      "Executing: JAVA_HOME={} GRAALVM_HOME={} {} {}",
      runtime.javaHome,
      runtime.javaHome,
      gu,
      command.mkString(" ")
    )

    val executor = new ExponentialBackoffRetry(5, logger) {
      override def cmd: String = "list"
      override def executeProcess(
        logger: ProcessLogger
      ): Try[LazyList[String]] = {
        val process = Process(
          updaterExec.toAbsolutePath.toString +: command,
          Some(runtime.javaHome.toFile),
          ("JAVA_HOME", runtime.javaHome),
          ("GRAALVM_HOME", runtime.javaHome)
        )
        Try(process.lazyLines(logger))
      }
    }
    executor
      .execute()
      .map(stdout => if (stdout.isEmpty) Seq() else ListOut.parse(stdout))
  }

  /** Install the provided GraalVM components.
    *
    * @param components the list of components to install
    */
  override def install(components: Seq[GraalVMComponent]): Try[Unit] = {
    if (components.nonEmpty) {
      val command = "install" +: components.map(_.id)
      logger.trace("{} {}", gu, Properties(gu))
      logger.debug(
        "Executing: JAVA_HOME={} GRRAALVM_HOME={} {} {}",
        runtime.javaHome,
        runtime.javaHome,
        gu,
        command.mkString(" ")
      )
      val executor = new ExponentialBackoffRetry(5, logger) {
        override def cmd: String = "install"
        override def executeProcess(
          logger: ProcessLogger
        ): Try[LazyList[String]] = {
          val process = Process(
            updaterExec.toAbsolutePath.toString +: command,
            Some(runtime.path.toFile),
            ("JAVA_HOME", runtime.javaHome),
            ("GRAALVM_HOME", runtime.javaHome)
          )
          Try(process.lazyLines(logger))
        }
      }
      executor.execute().map { stdout =>
        stdout.foreach(logger.trace(_))
        ()
      }
    } else {
      Success(())
    }
  }

}
object GraalVMComponentUpdater {

  abstract class ProcessWithRetries(maxRetries: Int, logger: Logger) {
    def executeProcess(logger: ProcessLogger): Try[LazyList[String]]

    def cmd: String

    def execute(): Try[List[String]] = execute(0)

    protected def retryWait(retry: Int): Long

    private def execute(retry: Int): Try[List[String]] = {
      val errors        = scala.collection.mutable.ListBuffer[String]()
      val processLogger = ProcessLogger(err => errors.addOne(err))
      executeProcess(processLogger) match {
        case Success(stdout) =>
          Try(stdout.toList).recoverWith({
            case _ if retry < maxRetries =>
              try {
                Thread.sleep(retryWait(retry))
              } catch {
                case _: InterruptedException =>
              }
              execute(retry + 1)
          })
        case Failure(exception) if retry < maxRetries =>
          logger.warn("{} failed: {}. Retrying...", cmd, exception.getMessage)
          try {
            Thread.sleep(retryWait(retry))
          } catch {
            case _: InterruptedException =>
          }
          execute(retry + 1)
        case Failure(exception) =>
          errors.foreach(logger.trace("[stderr] {}", _))
          Failure(exception)
      }
    }
  }

  abstract class ExponentialBackoffRetry(maxRetries: Int, logger: Logger)
      extends ProcessWithRetries(maxRetries, logger) {
    override def retryWait(retry: Int): Long = {
      200 * 2.toLong ^ retry
    }

  }

  implicit private def pathToString(path: Path): String =
    path.toAbsolutePath.toString

  /** Debug file properties. */
  private case class Properties(path: Path) {

    private val file = path.toFile

    override def toString: String =
      s"{ exists=${file.exists()}, " +
      s"executable=${file.canExecute} " +
      "}"
  }

  /** Parser for the `gu list -v` command output. */
  object ListOut {

    private val ID: String      = "ID"
    private val separator: Char = ':'

    /** Extract the GraalVM components from the gu output.
      *
      * @param lines the gu output
      * @return the list of GraalVM components.
      */
    def parse(lines: Seq[String]): Seq[GraalVMComponent] =
      lines
        .filter(_.startsWith(ID))
        .map(_.dropWhile(_ != separator).drop(1).trim)
        .map(GraalVMComponent(_))
  }
}
