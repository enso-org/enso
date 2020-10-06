import sbt.Keys._
import sbt._
import src.main.scala.licenses.backend.{
  CombinedBackend,
  GatherCopyrights,
  GatherNotices
}
import src.main.scala.licenses.frontend.SbtLicenses
import src.main.scala.licenses.report.Report
import src.main.scala.licenses.review.Review
import src.main.scala.licenses.{DependencySummary, DistributionDescription}

object GatherLicenses {
  val distributions = taskKey[Seq[DistributionDescription]](
    "Defines descriptions of distributions."
  )

  lazy val run = Def.task {
    val log  = state.value.log
    val root = target.value
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
      val (sbtInfo, sbtWarnings) =
        SbtLicenses.analyze(distribution.sbtComponents, log)
      sbtWarnings.foreach(log.warn(_))

      val allInfo = sbtInfo // TODO [RW] add Rust frontend result here (#1187)

      log.info(s"${allInfo.size} unique dependencies discovered")
      val backend = CombinedBackend(GatherNotices, GatherCopyrights)

      val processed = allInfo.map { dependency =>
        println(
          s"${dependency.moduleInfo} -> ${dependency.license} / ${dependency.url}"
        )
        val attachments = backend.run(dependency.sources)
        //println(s"Found: $attachments")
        (dependency, attachments)
      }

      val summary = DependencySummary(processed)
      val processedSummary =
        Review(file("legal-review") / distribution.artifactName, summary).run()
      val allWarnings       = sbtWarnings ++ processedSummary.warnings
      val reportDestination = root / s"${distribution.artifactName}-report.html"

      allWarnings.foreach(log.warn(_))

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
    }

    log.warn(
      "Finished gathering license information. " +
      "This is an automated process, make sure that its output is reviewed " +
      "by a human to ensure that all licensing requirements are met."
    )
  }

}
