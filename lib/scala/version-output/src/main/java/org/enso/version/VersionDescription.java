package org.enso.version;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Represents a description of a version string that can be rendered as a human-readable string or
 * in JSON format.
 */
public class VersionDescription {
  private final boolean includeRuntimeJVMInfo;
  private final boolean enableNativeImageOSWorkaround;
  private final String header;
  private final String version;
  private final String osArch;
  private final String osName;
  private final String osVersion;
  private final String vmName;
  private final String vmVendor;
  private final String jreVersion;
  private final List<VersionDescriptionParameter> additionalParameters;

  private VersionDescription(
      boolean includeRuntimeJVMInfo,
      boolean enableNativeImageOSWorkaround,
      String header,
      String version,
      List<VersionDescriptionParameter> additionalParameters) {
    this.includeRuntimeJVMInfo = includeRuntimeJVMInfo;
    this.enableNativeImageOSWorkaround = enableNativeImageOSWorkaround;
    this.header = header;
    this.version = version;
    this.additionalParameters = additionalParameters;

    this.osArch = System.getProperty("os.arch");
    this.osName = System.getProperty("os.name");
    this.osVersion = System.getProperty("os.version");

    this.vmName = System.getProperty("java.vm.name");
    this.vmVendor = System.getProperty("java.vm.vendor");
    this.jreVersion = System.getProperty("java.runtime.version");
  }

  public static VersionDescription make(
      String header, boolean includeRuntimeJVMInfo, boolean enableNativeImageOSWorkaround) {
    return make(header, includeRuntimeJVMInfo, enableNativeImageOSWorkaround, List.of(), null);
  }

  /**
   * Creates a {@link VersionDescription} instance.
   *
   * @param header header displayed as the first line of the human-readable representation
   * @param includeRuntimeJVMInfo if set to true, includes information about the JVM that is running
   *     the program
   * @param enableNativeImageOSWorkaround if set to true, changes how the OS information is
   *     displayed; this is a temporary workaround caused by the Native Image OS returning the value
   *     known at build-time and not at runtime
   * @param additionalParameters a sequence of additional {@link VersionDescriptionParameter} to
   *     include in the version string
   * @param customVersion if provided, overrides the version string from {@code GeneratedVersion};
   *     used for testing purposes
   * @return
   */
  public static VersionDescription make(
      String header,
      boolean includeRuntimeJVMInfo,
      boolean enableNativeImageOSWorkaround,
      List<VersionDescriptionParameter> additionalParameters,
      String customVersion) {
    Objects.requireNonNull(header);
    Objects.requireNonNull(additionalParameters);
    String version;
    if (customVersion != null) {
      version = customVersion;
    } else {
      version = BuildVersion.ensoVersion();
    }
    return new VersionDescription(
        includeRuntimeJVMInfo,
        enableNativeImageOSWorkaround,
        header,
        version,
        additionalParameters);
  }

  public String asHumanReadableString() {
    String runtimeDescription;
    if (includeRuntimeJVMInfo) {
      runtimeDescription =
          String.format(
              """
          Running on: %s, %s, JDK %s
                      %s, %s (%s)
          """,
              vmName, vmVendor, jreVersion, osName, osVersion, osArch);
    } else if (enableNativeImageOSWorkaround) {
      // TODO [RW] Currently the `os.name` property seems to be set to the
      //  OS the program has been built on, instead of the OS that is
      //  currently running. A workaround should be implemented in #1100
      //  that will use other means to query the OS name and version.
      runtimeDescription = String.format("Built on:   %s (%s)", osName, osArch);
    } else {
      runtimeDescription = String.format("Running on: %s %s (%s)", osName, osVersion, osArch);
    }
    String dirtyStr = "";
    if (BuildVersion.isDirty()) {
      dirtyStr = "*";
    }
    var parameters =
        formatParameters(VersionDescription::formatParameterAsHumanReadableString, "\n");
    return String.format(
        """
        %s
        Version:    %s
        Built with: scala-%s for GraalVM %s
        Built from: %s%s @ %s
        %s%s
        """,
        header,
        version,
        BuildVersion.scalacVersion(),
        BuildVersion.graalVersion(),
        BuildVersion.ref(),
        dirtyStr,
        BuildVersion.commit(),
        runtimeDescription,
        parameters);
  }

  public String asJSONString() {
    String runtimeDescription;
    if (includeRuntimeJVMInfo) {
      runtimeDescription =
          String.format(
              """
          "osName": "%s",
          "osVersion": "%s",
          "osArch": "%s",
          """,
              osName, osVersion, osArch);
    } else {
      runtimeDescription =
          String.format(
              """
          "vmName": "%s",
          "vmVendor": "%s",
          "jreVersion": "%s",
          "osName": "%s",
          "osVersion": "%s",
          "osArch": "%s",
          """,
              vmName, vmVendor, jreVersion, osName, osVersion, osArch);
    }
    var parameters = formatParameters(VersionDescription::formatParameterAsJSONString, ",\n");
    return String.format(
        """
        {
          "version": "%s",
          "ref": "%s",
          "dirty": "%s",
          "commit": "%s",
          %s%s
        }
        """,
        version,
        BuildVersion.ref(),
        BuildVersion.isDirty(),
        BuildVersion.commit(),
        runtimeDescription,
        parameters);
  }

  public String asString(boolean useJson) {
    if (useJson) {
      return asJSONString();
    } else {
      return asHumanReadableString();
    }
  }

  private static String formatParameterAsJSONString(VersionDescriptionParameter parameter) {
    return "\"" + parameter.jsonName() + "\": " + parameter.value() + "\"";
  }

  private static String formatParameterAsHumanReadableString(
      VersionDescriptionParameter parameter) {
    return parameter.humanReadableName() + ": " + parameter.value();
  }

  private String formatParameters(
      Function<VersionDescriptionParameter, String> formatter, String separator) {
    if (additionalParameters.isEmpty()) {
      return "";
    } else {
      return additionalParameters.stream().map(formatter).collect(Collectors.joining(separator));
    }
  }
}
