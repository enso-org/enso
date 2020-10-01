import com.typesafe.sbt.SbtLicenseReport.autoImportImpl.updateLicenses
import sbt.Keys._
import sbt._
// import sbt.internal.util.ManagedLogger

object GatherLicenses {
  lazy val run = Def.task {
    val log = state.value.log
    log.info(
      "Gathering license files and copyright notices. " +
      "This task may take a long time."
    )

    val licenseInfo = updateLicenses.value

    println(licenseInfo)

    log.warn(
      "Finished gathering license information. " +
      "This is an automated process, make sure that its output is reviewed " +
      "by a human to ensure that all licensing requirements are met."
    )
  }

}
