package org.enso.runner;

import org.enso.semver.SemVer;
import org.enso.version.BuildVersion;

/**
 * A helper class that allows to access current version of the runner.
 *
 * <p>The current version is parsed from {@link BuildVersion}, but in development mode it can be
 * overridden by setting `enso.version.override` property. This is used in project-manager tests to
 * override the version of projects created using the runner.
 */
final class CurrentVersion {
  private CurrentVersion() {}

  private static SemVer version = null;

  public static SemVer getVersion() {
    if (version == null) {
      version = computeVersion();
    }
    return version;
  }

  private static SemVer computeVersion() {
    var buildVersion =
        (SemVer)
            SemVer.parse(BuildVersion.ensoVersion())
                .getOrElse(
                    () -> {
                      throw new IllegalStateException(
                          "Fatal error: Enso version included in buildinfo is not a valid "
                              + "semver string, this should never happen.");
                    });
    if (!BuildVersion.isRelease()) {
      var overrideVersionProp = System.getProperty("enso.version.override");
      if (overrideVersionProp == null) {
        return buildVersion;
      }
      var parseRes = SemVer.parse(overrideVersionProp);
      if (parseRes.isSuccess()) {
        return parseRes.get();
      }
    }
    return buildVersion;
  }
}
