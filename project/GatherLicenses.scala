import com.typesafe.sbt.SbtLicenseReport.autoImportImpl.updateLicenses
import sbt.Keys._
import sbt._
// import sbt.internal.util.ManagedLogger

object GatherLicenses {
  val distributions = taskKey[Seq[DistributionDescription]](
    "Defines descriptions of distributions."
  )

  lazy val run = Def.task {
    val log = state.value.log
    log.info(
      "Gathering license files and copyright notices. " +
      "This task may take a long time."
    )

    for (distribution <- distributions.value) {
      log.info(s"Processing ${distribution.artifactName} distribution")
      val projectNames = distribution.sbtComponents.map(_.name)
      log.info(
        s"It consists of the following sbt project roots:" +
        s" ${projectNames.mkString(", ")}"
      )

      val combinedDependencies = (for {
        component <- distribution.sbtComponents
        dep       <- component.report.licenses
      } yield dep).distinct

      log.info(s"${combinedDependencies.length} unique dependencies found")

      for (dependency <- combinedDependencies) {
        println(s"${dependency.module} -> ${dependency.license}")
      }
    }

    log.warn(
      "Finished gathering license information. " +
      "This is an automated process, make sure that its output is reviewed " +
      "by a human to ensure that all licensing requirements are met."
    )
  }

}
