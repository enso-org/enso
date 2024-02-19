package org.enso.semver;

import com.github.zafarkhaja.semver.Version;

import java.util.Optional;
import scala.util.Try;

/**
 * A facade hiding the implementation of the Semantic Versioning used in Enso.
 */
public class SemVer {

    private Version version;

    private SemVer(Version version) {
        this.version = version;
    }

    public static Try<SemVer> parse(String version) {
        return Try.apply(() -> new SemVer(Version.parse(version)));
    }

    public static SemVer of(int major) {
        return new SemVer(Version.of(major));
    }

    public static SemVer of(int major, int minor) {
        return new SemVer(Version.of(major, minor));
    }

    public static SemVer of(int major, int minor, int patch) {
        return new SemVer(Version.of(major, minor, patch));
    }

    public static SemVer of(int major, int minor, int patch, String preReleaseVersion) {
        return new SemVer(Version.of(major, minor, patch, preReleaseVersion));
    }

    public boolean isGreaterThanOrEqual(SemVer v) {
        return version.isHigherThanOrEquivalentTo(v.version);
    }

    public boolean isGreaterThan(SemVer v) {
        return version.isHigherThan(v.version);
    }

    public boolean isLessThanOrEqual(SemVer v) {
        return version.isLowerThanOrEquivalentTo(v.version);
    }

    public boolean isLessThan(SemVer v) {
        return version.isLowerThan(v.version);
    }

    public Optional<String> preReleaseVersion() {
        return !version.isStable() ? version.preReleaseVersion() : Optional.empty();
    }

    public int compareTo(SemVer v) {
        return version.compareTo(v.version);
    }

    @Override
    public String toString() {
        return version.toString();
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

    @Override
    public int hashCode() {
        return version.hashCode();
    }
}
