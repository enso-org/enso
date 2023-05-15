package src.main.scala.licenses

import java.nio.file.Path

import sbtlicensereport.license.{DepModuleInfo, LicenseInfo}
import src.main.scala.licenses.report.Review

/** Defines a way to access sources of a dependency.
  *
  * This may involve various actions, such as downloading or extracting files
  * into a temporary directory.
  */
trait SourceAccess {

  /** Calls the callback with a path to the available sources.
    *
    * The provided path is only valid during the callbacks invocation and is
    * considered invalid as soon as that function completes.
    */
  def access[R](withSources: Path => R): R
}

/** Information about a single dependency.
  *
  * @param moduleInfo description of the module
  * @param license information about the module's discovered license
  * @param sources access to module's sources
  * @param url project URL (if available)
  */
case class DependencyInformation(
  moduleInfo: DepModuleInfo,
  license: LicenseInfo,
  sources: Seq[SourceAccess],
  url: Option[String]
) {

  /** Normalized name of the package that uniquely identifies the dependency.
    */
  def packageName: String =
    Review.normalizeName(
      moduleInfo.organization + "." + moduleInfo.name + "-" +
      moduleInfo.version
    )
}
