package src.main.scala.licenses

import com.typesafe.sbt.license.LicenseReport
import com.typesafe.sbt.license.SbtCompat.IvySbt
import sbt.librarymanagement.UpdateReport

case class SBTDistributionComponent(
  name: String,
  licenseReport: LicenseReport,
  ivyModule: IvySbt#Module,
  classifiedArtifactsReport: UpdateReport
)

case class DistributionDescription(
  artifactName: String,
  sbtComponents: Seq[SBTDistributionComponent]
) {
  def componentsNames: Seq[String] = sbtComponents.map(_.name)
}
