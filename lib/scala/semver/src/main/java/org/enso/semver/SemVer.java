package org.enso.semver;

import java.util.regex.Pattern;
import scala.util.Failure;
import scala.util.Success;
import scala.util.Try;

/** A facade hiding the implementation of the Semantic Versioning used in Enso. */
public final class SemVer {

  private final long major;
  private final long minor;
  private final long patch;
  private final String preRelease;
  private final String prefix;
  private final String buildMetadata;

  private static final Pattern regex =
      Pattern.compile(
          "^([a-z]+-)?(\\d|[1-9]\\d+)\\.(\\d|[1-9]\\d+)\\.(\\d|[1-9]\\d+)(-[0-9A-Za-z-\\.]+)?(\\+[0-9A-Za-z-\\.\\+]+)?$");

  private SemVer(
      long major, long minor, long patch, String preRelease, String prefix, String buildMetadata) {
    this.major = major;
    this.minor = minor;
    this.patch = patch;
    this.preRelease = preRelease;
    this.prefix = prefix;
    this.buildMetadata = buildMetadata;
  }

  private SemVer(long major, long minor, long patch, String preRelease) {
    this.major = major;
    this.minor = minor;
    this.patch = patch;
    this.preRelease = preRelease;
    this.prefix = null;
    this.buildMetadata = null;
  }

  private SemVer(long major, long minor, long patch) {
    this.major = major;
    this.minor = minor;
    this.patch = patch;
    this.preRelease = null;
    this.prefix = null;
    this.buildMetadata = null;
  }

  public static Try<SemVer> parse(String version) {
    var match = regex.matcher(version);
    if (match.find()) {
      var prefix = match.groupCount() > 0 ? match.group(1) : null;
      var preRelease =
          match.groupCount() == 6 && match.group(5) != null ? match.group(5).substring(1) : null;
      var buildMetadata =
          match.groupCount() == 6 && match.group(6) != null ? match.group(6).substring(1) : null;
      return Success.apply(
          new SemVer(
              Integer.valueOf(match.group(2)),
              Integer.valueOf(match.group(3)),
              Integer.valueOf(match.group(4)),
              preRelease,
              prefix,
              buildMetadata));

    } else {
      return Failure.apply(new RuntimeException("Invalid version " + version));
    }
  }

  public static SemVer of(int major) {
    return new SemVer(major, 0, 0);
  }

  public static SemVer of(int major, int minor) {
    return new SemVer(major, minor, 0);
  }

  public static SemVer of(int major, int minor, int patch) {
    return new SemVer(major, minor, patch);
  }

  public static SemVer of(int major, int minor, int patch, String preReleaseVersion) {
    return new SemVer(major, minor, patch, preReleaseVersion);
  }

  public boolean isGreaterThanOrEqual(SemVer v) {
    return compareTo(v) >= 0;
  }

  public boolean isGreaterThan(SemVer v) {
    return compareTo(v) > 0;
  }

  public boolean isLessThanOrEqual(SemVer v) {
    return compareTo(v) <= 0;
  }

  public boolean isLessThan(SemVer v) {
    return compareTo(v) < 0;
  }

  public String preReleaseVersion() {
    return preRelease;
  }

  public String getBuildMetadata() {
    return buildMetadata;
  }

  /**
   * Compares two versions. Comparison ignores an optional prefix or build metadata suffix.
   *
   * @param v version to compare to
   * @return -1 if this version is less than `v`, 1 if it is bigger than `v`, and 0 if they are the
   *     same
   */
  public int compareTo(SemVer v) {
    if (major < v.major) return -1;
    else if (major > v.major) return 1;
    else {
      if (minor < v.minor) return -1;
      else if (minor > v.minor) return 1;
      else {
        if (patch < v.patch) return -1;
        else if (patch > v.patch) return 1;
        else {
          if (preRelease == null && v.preRelease == null) return 0;
          if (preRelease == null) return 1;
          else if (v.preRelease == null) return -1;
          else return (preRelease.equals(v.preRelease) ? 0 : preRelease.compareTo(v.preRelease));
        }
      }
    }
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    if (prefix != null) sb.append(prefix);
    sb.append(major + ".");
    sb.append(minor + ".");
    sb.append(patch);
    if (preRelease != null) sb.append("-" + preRelease);
    return sb.toString();
  }

  @Override
  public boolean equals(Object other) {
    if (this == other) {
      return true;
    }
    if (!(other instanceof SemVer)) {
      return false;
    }
    return compareTo((SemVer) other) == 0;
  }
}
