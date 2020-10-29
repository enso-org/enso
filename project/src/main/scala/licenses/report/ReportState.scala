package src.main.scala.licenses.report

import java.nio.file.Files
import java.security.MessageDigest

import sbt.internal.util.ManagedLogger
import sbt.{File, IO}
import src.main.scala.licenses.{
  DistributionDescription,
  FilesHelper,
  PortablePath
}

import scala.util.control.NonFatal

/** Describes the current state of legal review report.
  *
  * @param inputHash hash (in hexadecimal format) of the list of dependencies;
  *                  used to detect if a dependency has been added or removed
  * @param outputHash hash (in hexadecimal format) of the generated notice
  *                   packages; used to make sure that the generated files have
  *                   not been edited or removed by mistake
  * @param warningsCount the amount of warnings; used to check if any new issues
  *                      have been resolved
  */
case class ReportState(
  inputHash: String,
  outputHash: String,
  warningsCount: Int
)

object ReportState {

  /** Reads a [[ReportState]] from the file at the provided path.
    *
    * If the file does not exist or cannot be read, None is returned and it is
    * treated as if the report has not been generated at all.
    *
    * The file consists of three lines: input hash, output hash and the warning
    * count.
    */
  def read(file: File, log: ManagedLogger): Option[ReportState] = {
    try {
      IO.readLines(file) match {
        case List(inputHash, outputHash, count) =>
          Some(ReportState(inputHash, outputHash, count.toInt))
        case _ =>
          log.error(s"Review state at $file is malformed.")
          None
      }
    } catch {
      case NonFatal(error) =>
        log.error(s"Could not read review state at $file: $error")
        None
    }
  }

  /** Writes the [[ReportState]] to a file.
    *
    * See [[read]] for file format description.
    */
  def write(file: File, reportState: ReportState): Unit = {
    IO.createDirectory(file.getParentFile)
    IO.write(
      file,
      Seq(
        reportState.inputHash,
        reportState.outputHash,
        reportState.warningsCount
      ).mkString("", "\n", "\n")
    )
  }

  /** Computes a hash of the [[DistributionDescription]] used as input to the
    * license gathering process.
    */
  def computeInputHash(
    distributionDescription: DistributionDescription
  ): String = {
    val DistributionDescription(
      artifactName,
      _,
      sbtComponents
    ) = distributionDescription

    val digest = MessageDigest.getInstance("SHA-256")
    digest.update(artifactName.getBytes)
    for (sbtComponent <- sbtComponents) {
      digest.update(sbtComponent.name.getBytes)
      val dependencies =
        sbtComponent.licenseReport.licenses.sortBy(_.module.toString)
      for (dep <- dependencies) {
        digest.update(dep.module.toString.getBytes)
        digest.update(dep.license.name.getBytes)
      }
    }
    hexString(digest.digest())
  }

  /** Computes a hash of all files included in the generated notice package. */
  def computeOutputHash(
    distributionDescription: DistributionDescription
  ): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    val root   = distributionDescription.packageDestination.toPath
    val allFiles =
      FilesHelper
        .walk(root)(Seq(_))
        .map(p => PortablePath(root.relativize(p)))
        .sortBy(_.toString)
    for (path <- allFiles) {
      digest.update(path.toString.getBytes)
      val file = root.resolve(path.path).toFile
      if (!file.isDirectory) {
        digest.update(IO.readBytes(file))
      }
    }
    hexString(digest.digest())
  }

  /** Returns a byte array encoded as a hexadecimal number. */
  private def hexString(bytes: Array[Byte]): String =
    bytes.map("%02X".format(_)).mkString

  /** Writes an updated [[ReportState]] based on current report status.
    *
    * @param file destination to write to
    * @param distributionDescription description of the distribution, used to
    *                                compute the input hash and locate the
    *                                generated output
    * @param warningsCount amount of warnings present in the current report
    */
  def write(
    file: File,
    distributionDescription: DistributionDescription,
    warningsCount: Int
  ): Unit = {
    val state = ReportState(
      inputHash     = computeInputHash(distributionDescription),
      outputHash    = computeOutputHash(distributionDescription),
      warningsCount = warningsCount
    )
    write(file, state)
  }
}
