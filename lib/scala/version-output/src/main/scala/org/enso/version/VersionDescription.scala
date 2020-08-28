package org.enso.version

import buildinfo.Info

trait VersionDescription {
  def asHumanReadableString: String
  def asJSONString:          String

  def asString(useJson: Boolean): String =
    if (useJson) asJSONString else asHumanReadableString
}

/**
  * Defines an additional parameter for the version description.
  *
  * @param humanReadableName the human readable prefix added when printing this
  *                          parameter in human-readable format
  * @param jsonName the key when outputting the parameter in JSON format
  * @param value the value to use for the parameter; depending on if the whole
  *              version description will be queried as a human-readable version
  *              or in JSON, this value should be in the right format
  */
case class VersionDescriptionParameter(
  humanReadableName: String,
  jsonName: String,
  value: String
)

object VersionDescription {
  def formatParameterAsJSONString(
    parameter: VersionDescriptionParameter
  ): String =
    s""""${parameter.jsonName}": ${parameter.value}"""
  def formatParameterAsHumanReadableString(
    parameter: VersionDescriptionParameter
  ): String =
    s"${parameter.humanReadableName}: ${parameter.value}"

  def make(
    header: String,
    includeRuntimeJVMInfo: Boolean,
    enableNativeImageOSWorkaround: Boolean                 = false,
    additionalParameters: Seq[VersionDescriptionParameter] = Seq.empty
  ): VersionDescription = {
    val version   = Info.ensoVersion
    val osArch    = System.getProperty("os.arch")
    val osName    = System.getProperty("os.name")
    val osVersion = System.getProperty("os.version")

    val vmName     = System.getProperty("java.vm.name")
    val vmVendor   = System.getProperty("java.vm.vendor")
    val jreVersion = System.getProperty("java.runtime.version")

    def formatParameters(
      formatter: VersionDescriptionParameter => String,
      separator: String
    ): String =
      if (additionalParameters.isEmpty) ""
      else separator + additionalParameters.map(formatter).mkString(separator)

    new VersionDescription {
      override def asHumanReadableString: String = {
        val runtimeDescription =
          if (includeRuntimeJVMInfo)
            s"""Running on: $vmName, $vmVendor, JDK $jreVersion
               |            $osName $osVersion ($osArch)""".stripMargin
          else if (enableNativeImageOSWorkaround) {
            // TODO [RW] Currently the `os.name` property seems to be set to the
            //  OS the program has been built on, instead of the OS that is
            //  currently running. A workaround should be implemented in #1100
            //  that will use other means to query the OS name and version.
            s"Built on:   $osName ($osArch)"
          } else s"Running on: $osName $osVersion ($osArch)"

        val dirtyStr = if (Info.isDirty) "*" else ""
        val parameters =
          formatParameters(formatParameterAsHumanReadableString, "\n")
        s"""$header
           |Version:    $version
           |Built with: scala-${Info.scalacVersion} for GraalVM ${Info.graalVersion}
           |Built from: ${Info.ref}$dirtyStr @ ${Info.commit}
           |$runtimeDescription$parameters
           |""".stripMargin
      }

      override def asJSONString: String = {
        val runtimeDescription =
          if (includeRuntimeJVMInfo)
            s"""  "osName": "$osName",
               |  "osVersion": "$osVersion",
               |  "osArch": "$osArch"""".stripMargin
          else
            s"""  "vmName": "$vmName",
               |  "vmVendor": "$vmVendor",
               |  "jreVersion": "$jreVersion",
               |  "osName": "$osName",
               |  "osVersion": "$osVersion",
               |  "osArch": "$osArch"""".stripMargin
        val parameters = formatParameters(formatParameterAsJSONString, ",\n")
        s"""{ "version": "$version",
           |  "ref": "${Info.ref}",
           |  "dirty": ${Info.isDirty},
           |  "commit": "${Info.commit}",
           |  $runtimeDescription$parameters
           |}""".stripMargin
      }
    }
  }
}
