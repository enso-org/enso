package org.enso.semver;

import java.util.Optional;
import java.util.regex.Pattern;

import scala.util.Failure;
import scala.util.Success;
import scala.util.Try;

/**
 * A facade hiding the implementation of the Semantic Versioning used in Enso.
 */
public class SemVer {

    private long major;
    private long minor;
    private long patch;
    private Optional<String> preRelease;
    private Optional<String> prefix;

    private static final Pattern regex = Pattern.compile("([a-z]+-)?(\\d+)\\.(\\d+)\\.(\\d+)(-[a-z]+)?");

    private SemVer(long major, long minor, long patch, Optional<String> preRelease, Optional<String> prefix) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.preRelease = preRelease;
        this.prefix = prefix;
    }

    private SemVer(long major, long minor, long patch, Optional<String> preRelease) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.preRelease = preRelease;
        this.prefix = Optional.empty();
    }

    private SemVer(long major, long minor, long patch) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
        this.preRelease = Optional.empty();
        this.prefix = Optional.empty();
    }

    public static Try<SemVer> parse(String version) {
        var match = regex.matcher(version);
        if (match.find()) {
            Optional<String> prefix = match.groupCount() > 0 && match.group(1) != null ? Optional.of(match.group(1)) : Optional.empty();
            Optional<String> preRelease = match.groupCount() == 5 && match.group(5) != null ? Optional.of(match.group(5).substring(1)) : Optional.empty();
            return Success.apply(new SemVer(
                    Integer.valueOf(match.group(2)),
                    Integer.valueOf(match.group(3)),
                    Integer.valueOf(match.group(4)),
                    preRelease,
                    prefix));

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
        return new SemVer(major, minor, patch, Optional.of(preReleaseVersion));
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

    public Optional<String> preReleaseVersion() {
        return preRelease;
    }


    /**
     * Compares two versions.
     * Comparison ignores an optional prefix.
     *
     * @param v version to compare to
     * @return -1 if this version is less than `v`, 1 if it is bigger than `v`, and 0 if they are the same
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
                    if (preRelease.isEmpty() && v.preRelease.isEmpty()) return 0;
                    if (preRelease.isEmpty()) return 1;
                    else if (v.preRelease.isEmpty()) return -1;
                    else return (preRelease.equals(v.preRelease) ? 0 : preRelease.get().compareTo(v.preRelease.get()));
                }
            }
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        prefix.ifPresent(v -> sb.append(v));
        sb.append(major + ".");
        sb.append(minor + ".");
        sb.append(patch);
        preRelease.ifPresent(v -> sb.append("-" + v));
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
