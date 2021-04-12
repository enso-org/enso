package org.enso.runtimeversionmanager.components

import java.nio.file.Path

import org.enso.runtimeversionmanager.OS
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.{Failure, Success}

class GraalVMComponentUpdaterSpec extends AnyWordSpec with Matchers {

  val javaHomeOpt: Option[Path] = sys.env.get("JAVA_HOME").map(Path.of(_))
  val os: OS                    = OS.operatingSystem

  val vendorVersionOpt: Option[String] =
    sys.props.get("java.vendor.version").map(_.dropWhile(!_.isDigit))

  val javaVersionOpt: Option[String] =
    sys.props.get("java.specification.version")

  val graalVMVersionOpt: Option[GraalVMVersion] =
    for {
      graalVersion <- vendorVersionOpt
      javaVersion  <- javaVersionOpt
    } yield GraalVMVersion(graalVersion, javaVersion)

  val graalRuntime: Option[GraalRuntime] =
    for {
      javaHome       <- javaHomeOpt
      graalVMVersion <- graalVMVersionOpt
    } yield GraalRuntime(graalVMVersion, javaHome)

  def isCI: Boolean = sys.env.contains("CI")

  def getOrElseFail[A](opt: Option[A]): A = {
    if (opt.isEmpty) {
      if (isCI) {
        throw new Exception(
          s"Error in test environment. " +
          s"OS=$os; " +
          s"JAVA_HOME=$javaHomeOpt; " +
          s"java.vendor.version=$vendorVersionOpt; " +
          s"java.specification.version=$javaVersionOpt"
        )
      } else {
        pending
      }
    }
    opt.get
  }

  "RuntimeComponentUpdater" should {

    "list components" in {
      val graal = getOrElseFail(graalRuntime)
      val ru    = new GraalVMComponentUpdater(graal, os)

      ru.list match {
        case Success(components) =>
          components should not be empty
        case Failure(cause) =>
          fail(cause)
      }
    }
  }

}
