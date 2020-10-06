package src.main.scala.licenses

import java.nio.file.Path

import com.typesafe.sbt.license.{DepModuleInfo, LicenseInfo}
import src.main.scala.licenses.review.Review

trait SourceAccess {
  def access[R](withSources: Path => R): R
}

case class DependencyInformation(
  moduleInfo: DepModuleInfo,
  license: LicenseInfo,
  sources: Seq[SourceAccess],
  url: Option[String]
) {
  def packageName: String =
    Review.normalizeName(
      moduleInfo.organization + "." + moduleInfo.name + "-" +
      moduleInfo.version
    )
}
