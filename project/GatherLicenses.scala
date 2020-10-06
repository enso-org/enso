import sbt.Keys._
import sbt._
import src.main.scala.licenses.backend.{
  CombinedBackend,
  GatherCopyrights,
  GatherNotices
}
import src.main.scala.licenses.frontend.SbtLicenses
import src.main.scala.licenses.report.{PackageNotices, Report}
import src.main.scala.licenses.review.Review
import src.main.scala.licenses.{DependencySummary, DistributionDescription}

object GatherLicenses {
  val distributions = taskKey[Seq[DistributionDescription]](
    "Defines descriptions of distributions."
  )
  val configurationRoot = settingKey[File]("Path to review configuration.")
  val distributionRoot = settingKey[File](
    "Path that will contain generated notices for each artefact."
  )

  lazy val run = Def.task {
    val log        = state.value.log
    val targetRoot = target.value
    log.info(
      "Gathering license files and copyright notices. " +
      "This task may take a long time."
    )

    val configRoot    = configurationRoot.value
    val generatedRoot = distributionRoot.value

    for (distribution <- distributions.value) {
      log.info(s"Processing ${distribution.artifactName} distribution")
      val projectNames = distribution.sbtComponents.map(_.name)
      log.info(
        s"It consists of the following sbt project roots:" +
        s" ${projectNames.mkString(", ")}"
      )
      val (sbtInfo, sbtWarnings) =
        SbtLicenses.analyze(distribution.sbtComponents, log)
      sbtWarnings.foreach(log.warn(_))

      val allInfo = sbtInfo // TODO [RW] add Rust frontend result here (#1187)

      log.info(s"${allInfo.size} unique dependencies discovered")
      val backend = CombinedBackend(GatherNotices, GatherCopyrights)

      val processed = allInfo.map { dependency =>
        log.debug(
          s"Processing ${dependency.moduleInfo} -> " +
          s"${dependency.license} / ${dependency.url}"
        )
        val attachments = backend.run(dependency.sources)
        (dependency, attachments)
      }

      val summary = DependencySummary(processed)
      val processedSummary =
        Review(configRoot / distribution.artifactName, summary).run()
      val summaryWarnings = processedSummary.warnings
      val allWarnings     = sbtWarnings ++ summaryWarnings
      val reportDestination =
        targetRoot / s"${distribution.artifactName}-report.html"

      sbtWarnings.foreach(log.warn(_))
      if (summaryWarnings.size > 10)
        log.warn(
          s"There are too many warnings (${summaryWarnings.size}) to " +
          s"display. Please inspect the generated report."
        )
      else allWarnings.foreach(log.warn(_))

      Report.writeHTML(
        distribution,
        processedSummary,
        allWarnings,
        reportDestination
      )
      log.info(
        s"Written the report for ${distribution.artifactName} to " +
        s"`${reportDestination}`."
      )
      val packagePath =
        generatedRoot / distribution.artifactName / "THIRD-PARTY"
      PackageNotices.create(
        distribution,
        processedSummary,
        packagePath
      )
      log.info(s"Re-generated distribution notices at `$packagePath`.")
      if (summaryWarnings.nonEmpty) {
        log.error(
          "The distribution notices were regenerated, but there are " +
          "not-reviewed issues within the report. The notices are probably " +
          "incomplete."
        )
      }
    }

    log.warn(
      "Finished gathering license information. " +
      "This is an automated process, make sure that its output is reviewed " +
      "by a human to ensure that all licensing requirements are met."
    )
  }

}
