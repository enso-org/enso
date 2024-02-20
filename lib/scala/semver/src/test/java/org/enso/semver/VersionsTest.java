package org.enso.semver;

import static org.junit.Assert.*;
import org.junit.Test;
public class VersionsTest {

    @Test
    public void testVersionsParsing() throws Exception {
        assertTrue(SemVer.parse("0.1.0").isSuccess());
        assertTrue(SemVer.parse("0.1.1").isSuccess());
        assertTrue(SemVer.parse("9999.0.0").isSuccess());
        assertTrue(SemVer.parse("0.1").isFailure());
    }

    @Test
    public void testVersionsParsingForPreReleases() throws Exception {
        assertTrue(SemVer.parse("0.1.0-foo").isSuccess());
        assertTrue(SemVer.parse("0.1.1-broken").isSuccess());
        assertTrue(SemVer.parse("0.0.0-dev").isSuccess());
        var r3 = SemVer.parse("enso-0.1.0");
        assertTrue(r3.isSuccess());
        assertTrue(r3.get().preReleaseVersion().isEmpty());
        assertTrue(r3.get().equals(SemVer.of(0, 1, 0)));
        var r4 = SemVer.parse("enso-0.1.0-b");
        assertTrue(r4.isSuccess());
        assertTrue(!r4.get().preReleaseVersion().isEmpty());
        assertTrue(!r4.get().equals(SemVer.of(0, 1, 0)));
        assertEquals("parsed version should preserve prefix", r4.get().toString(), "enso-0.1.0-b");
    }

    @Test
    public void testVersionComparison() throws Exception {
        var v1 = SemVer.parse("9999.0.0").get();
        var v2 = SemVer.parse("0.0.0").get();
        assertTrue(v1.isGreaterThan(v2));
        assertTrue(!v1.isLessThan(v2));

        var v3 = SemVer.parse("0.0.0-dev").get();
        var v4 = SemVer.parse("0.0.0-dev").get();
        assertFalse(v3 == v4);
        assertTrue(v3.equals(v4));
        assertEquals(v3.compareTo(v4), 0);

        assertEquals(v2.compareTo(v3), 1);
        assertEquals(v3.compareTo(v2), -1);
    }
}
