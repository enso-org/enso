import com.typesafe.sbt.license.LicenseReport

case class DistributionComponent(name: String, report: LicenseReport)

case class DistributionDescription(
  artifactName: String,
  sbtComponents: Seq[DistributionComponent]
)
