import sbt.Keys._
import sbt._
import complete.DefaultParsers._
import org.apache.ivy.core.resolve.IvyNode
import src.main.scala.licenses.backend.{
  CombinedBackend,
  GatherCopyrights,
  GatherNotices,
  GithubHeuristic
}
import src.main.scala.licenses.frontend.SbtLicenses
import src.main.scala.licenses.report._
import src.main.scala.licenses.{DependencySummary, DistributionDescription}

import scala.collection.JavaConverters._
import scala.sys.process._

/** The task and configuration for automatically gathering license information.
  */
object GatherLicenses {
  val distributions = taskKey[Seq[DistributionDescription]](
    "Defines descriptions of distributions."
  )
  val configurationRoot = settingKey[File]("Path to review configuration.")
  val licenseConfigurations =
    settingKey[Set[String]]("The ivy configurations we consider in the review.")
  private val stateFileName = "report-state"

  /** The task that performs the whole license gathering process. */
  lazy val run = Def.task {
    val log        = state.value.log
    val targetRoot = target.value
    log.info(
      "Gathering license files and copyright notices. " +
      "This task may take a long time."
    )

    val configRoot = configurationRoot.value

    val reports = distributions.value.map { distribution =>
      log.info(s"Processing the ${distribution.artifactName} distribution")
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
      val defaultBackend = CombinedBackend(GatherNotices, GatherCopyrights)

      val processed = allInfo.map { dependency =>
        log.debug(
          s"Processing ${dependency.moduleInfo} (${dependency.license}) -> " +
          s"${dependency.url}"
        )
        val defaultAttachments = defaultBackend.run(dependency.sources)
        val attachments =
          if (defaultAttachments.nonEmpty) defaultAttachments
          else GithubHeuristic(dependency, log).run()
        (dependency, attachments)
      }

      val summary          = DependencySummary(processed)
      val distributionRoot = configRoot / distribution.artifactName
      val WithWarnings(processedSummary, summaryWarnings) =
        Review(distributionRoot, summary).run()
      val allWarnings = sbtWarnings ++ summaryWarnings
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
        s"Written the report for the ${distribution.artifactName} to " +
        s"`$reportDestination`."
      )
      val packagePath = distribution.packageDestination
      PackageNotices.create(distribution, processedSummary, packagePath)
      ReportState.write(
        distributionRoot / stateFileName,
        distribution,
        summaryWarnings.length
      )
      log.info(s"Re-generated distribution notices at `$packagePath`.")
      if (summaryWarnings.nonEmpty) {
        log.warn(
          "The distribution notices were regenerated, but there are " +
          "not-reviewed issues within the report. The notices are probably " +
          "incomplete."
        )
      }

      (distribution, processedSummary)
    }

    log.warn(
      "Finished gathering license information. " +
      "This is an automated process, make sure that its output is reviewed " +
      "by a human to ensure that all licensing requirements are met."
    )

    reports
  }

  /** The task that verifies if the report has been generated and is up-to-date.
    */
  lazy val verifyReports = Def.task {
    val configRoot = configurationRoot.value
    val log        = streams.value.log
    def warnAndThrow(exceptionMessage: String): Nothing = {
      log.error(exceptionMessage)
      log.warn(
        "Please make sure to run `enso / gatherLicenses` " +
        "and review any changed dependencies, " +
        "ensuring that the review is complete and there are no warnings."
      )
      throw LegalReviewException(exceptionMessage)
    }

    for (distribution <- distributions.value) {
      val distributionRoot = configRoot / distribution.artifactName
      val name             = distribution.artifactName
      ReportState.read(distributionRoot / stateFileName, log) match {
        case Some(reviewState) =>
          val currentInputHash = ReportState.computeInputHash(distribution)
          if (currentInputHash != reviewState.inputHash) {
            warnAndThrow(
              s"Report for the $name is not up to date - " +
              s"it seems that some dependencies were added or removed."
            )
          }

          if (reviewState.warningsCount > 0) {
            warnAndThrow(
              s"Report for the $name has ${reviewState.warningsCount} warnings."
            )
          }

          val currentOutputHash = ReportState.computeOutputHash(distribution)
          if (currentOutputHash != reviewState.outputHash) {
            log.error(
              s"Report for the $name seems to be up-to-date but the notice " +
              s"package has been changed."
            )
            log.warn(
              "Re-run `enso / gatherLicenses` and make sure that all files " +
              "from the notice package are committed and no unexpected files " +
              "have been added."
            )
            throw LegalReviewException(
              s"Output directory for $name has different content than expected."
            )
          }

          log.info(s"Report and package for $name are reviewed and up-to-date.")
        case None =>
          warnAndThrow(s"Report for $name has not been generated.")
      }
    }
  }

  case class LegalReviewException(string: String)
      extends RuntimeException(string)

  /** Launches a server that allows to easily review the generated report.
    *
    * Requires `npm` to be on the system PATH.
    */
  def runReportServer(): Unit = {
    Seq("npm", "install").!
    Process(Seq("npm", "start"), file("tools/legal-review-helper"))
      .run(connectInput = true)
      .exitValue()
  }

  /** A task that prints which sub-projects use a dependency and what
    * dependencies use it (so that one can track where dependencies come from).
    */
  lazy val analyzeDependency = Def.inputTask {
    val args: Seq[String]      = spaceDelimited("<arg>").parsed
    val evaluatedDistributions = distributions.value
    val log                    = streams.value.log
    for (arg <- args) {
      for (distribution <- evaluatedDistributions) {
        for (sbtComponent <- distribution.sbtComponents) {
          val ivyDeps =
            sbtComponent.licenseReport.orig.getDependencies.asScala
              .map(_.asInstanceOf[IvyNode])
          for (dep <- sbtComponent.licenseReport.licenses) {
            if (dep.module.name.contains(arg)) {
              val module = dep.module
              log.info(
                s"${distribution.artifactName} distribution, project ${sbtComponent.name} " +
                s"contains $module"
              )
              val node = ivyDeps.find(n =>
                SbtLicenses.safeModuleInfo(n) == Some(dep.module)
              )
              node match {
                case None =>
                  log.warn(s"IvyNode for $module not found.")
                case Some(ivyNode) =>
                  val callers =
                    ivyNode.getAllCallers.toSeq.map(_.toString).distinct
                  log.info(s"Callers: $callers")
              }
            }
          }
        }
      }
    }
  }
}
