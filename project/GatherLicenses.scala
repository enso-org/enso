import com.typesafe.sbt.SbtLicenseReport.autoImportImpl.updateLicenses
import sbt.Keys._
import sbt._
import src.main.scala.licenses.DistributionDescription
import src.main.scala.licenses.backend.{CombinedBackend, GatherNotices}
import src.main.scala.licenses.frontend.{DependencyFilter, SbtLicenses}
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
      val (sbtInfo, sbtWarnings) =
        SbtLicenses.analyze(distribution.sbtComponents, log)
      sbtWarnings.foreach(log.warn(_))

      val allInfo = sbtInfo // TODO [RW] add Rust frontend result here (#1187)

      log.info(s"${allInfo.size} unique dependencies discovered")
      val backend = CombinedBackend(GatherNotices)

      allInfo.foreach { dependency =>
        println(
          s"${dependency.moduleInfo} -> ${dependency.license} / ${dependency.url}"
        )
        val attachments = backend.run(dependency.sources)
        println(s"Found: $attachments")
      }

    }

    log.warn(
      "Finished gathering license information. " +
      "This is an automated process, make sure that its output is reviewed " +
      "by a human to ensure that all licensing requirements are met."
    )
  }

}
