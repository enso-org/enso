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

  private def verifyReportStatus(
    log: Logger,
    targetRoot: File,
    distributionDescription: DistributionDescription,
    distributionConfig: File
  ): Unit = {
    val name = distributionDescription.artifactName

    def warnAndThrow(exceptionMessage: String): Nothing = {
      log.error(exceptionMessage)
      log.warn(
        "Please make sure to run `enso / gatherLicenses` " +
        s"and review the reports generated at $targetRoot, " +
        "ensuring that the legal review is complete and there are no warnings."
      )
      log.warn(
        "See docs/distribution/licenses.md#review-process for a more detailed " +
        "explanation."
      )
      throw LegalReviewException(exceptionMessage)
    }

    ReportState.read(distributionConfig / stateFileName, log) match {
      case Some(reviewState) =>
        val currentInputHash =
          ReportState.computeInputHash(distributionDescription)
        if (currentInputHash != reviewState.inputHash) {
          log.info("Input hash computed from build.sbt: " + currentInputHash)
          log.info("Input hash stored in metadata: " + reviewState.inputHash)
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

        log.info(s"Report for $name is reviewed.")
      case None =>
        warnAndThrow(s"Report for $name has not been generated.")
    }
  }

  private def verifyPackage(
    log: Logger,
    distributionConfig: File,
    packageDestination: File
  ): Unit = {
    val reportState = ReportState
      .read(distributionConfig / stateFileName, log)
      .getOrElse(
        throw LegalReviewException(
          s"Report at $distributionConfig is not available. " +
          s"Make sure to run `enso/gatherLicenses`."
        )
      )

    val currentOutputHash = ReportState.computeOutputHash(packageDestination)
    if (currentOutputHash != reportState.outputHash) {
      log.info("Output hash computed from build.sbt: " + currentOutputHash)
      log.info("Output hash stored in metadata: " + reportState.outputHash)
      log.error(
        s"Generated package at $packageDestination seems to be not up-to-date."
      )
      log.warn(
        "Re-run `enso/gatherLicenses` and make sure that all files " +
        "from the notice package are committed, no unexpected files " +
        "have been added and the package is created in a consistent way."
      )
      throw LegalReviewException(
        s"Package $packageDestination has different content than expected."
      )
    } else {
      log.info(s"Package $packageDestination is up-to-date.")
    }
  }

  /** The task that verifies if the report has been generated and is up-to-date.
    */
  lazy val verifyReports = Def.task {
    val configRoot = configurationRoot.value
    val log        = streams.value.log
    val targetRoot = target.value

    for (distribution <- distributions.value) {
      val distributionConfig = configRoot / distribution.artifactName
      verifyReportStatus(
        log                     = log,
        targetRoot              = targetRoot,
        distributionDescription = distribution,
        distributionConfig      = distributionConfig
      )
      verifyPackage(
        log                = log,
        distributionConfig = distributionConfig,
        packageDestination = distribution.packageDestination
      )
    }
  }

  /** A task that verifies if contents of the provided package directory are
    * up-to-date with the review state.
    *
    * It takes two arguments:
    * - an artifact name identifying the distribution
    * - a path to the generated packages.
    */
  lazy val verifyGeneratedPackage = Def.inputTask {
    val configRoot        = configurationRoot.value
    val log               = streams.value.log
    val args: Seq[String] = spaceDelimited("<arg>").parsed
    val (distributionName, packagePathString) = args match {
      case Seq(distribution, path) => (distribution, path)
      case _ =>
        throw new IllegalArgumentException(
          "The task expects exactly 2 arguments."
        )
    }
    val packageDestination = file(packagePathString)
    verifyPackage(
      log                = log,
      distributionConfig = configRoot / distributionName,
      packageDestination = packageDestination
    )
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
