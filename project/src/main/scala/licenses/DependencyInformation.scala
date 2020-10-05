package src.main.scala.licenses

import java.nio.file.Path

import com.typesafe.sbt.license.{DepModuleInfo, LicenseInfo}

trait SourceAccess {
  def access[R](withSources: Path => R): R
}

case class DependencyInformation(
  moduleInfo: DepModuleInfo,
  license: LicenseInfo,
  sources: Seq[SourceAccess],
  url: Option[String]
)
