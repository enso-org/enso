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
    assertTrue(SemVer.parse("21.0.2").isSuccess());
    assertTrue(SemVer.parse("17.0.7").isSuccess());
  }

  @Test
  public void testVersionsParsingForPreReleases() throws Exception {
    assertTrue(SemVer.parse("0.1.0-foo").isSuccess());
    assertTrue(SemVer.parse("0.1.1-broken").isSuccess());
    assertTrue(SemVer.parse("0.0.0-dev").isSuccess());
    var r3 = SemVer.parse("enso-0.1.0");
    assertTrue(r3.isSuccess());
    assertTrue(r3.get().preReleaseVersion() == null);
    assertTrue(r3.get().equals(SemVer.of(0, 1, 0)));
    var r4 = SemVer.parse("enso-0.1.0-b");
    assertTrue(r4.isSuccess());
    assertTrue(r4.get().preReleaseVersion() != null);
    assertTrue(!r4.get().equals(SemVer.of(0, 1, 0)));
    assertEquals("parsed version should preserve prefix", r4.get().toString(), "enso-0.1.0-b");
    var r5 = SemVer.parse("1.2.3+21");
    assertTrue(r5.isSuccess());
    assertTrue(r5.get().preReleaseVersion() == null);
    assertTrue(r5.get().getBuildMetadata().equals("21"));
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
    assertTrue(v3.isLessThan(v2));
    assertTrue(v3.isLessThanOrEqual(v2));
    assertTrue(v2.isGreaterThan(v3));

    assertTrue(SemVer.of(1, 2, 3).isLessThan(SemVer.of(4, 0, 0)));
    assertTrue(SemVer.of(4, 0, 0).isGreaterThan(SemVer.of(1, 0, 0)));
    assertTrue(SemVer.of(2, 9, 9).isLessThan(SemVer.of(3, 1, 1)));
    assertTrue(SemVer.of(2, 2, 1).isGreaterThan(SemVer.of(2, 2, 0)));
    assertTrue(SemVer.of(2, 3, 0).isGreaterThan(SemVer.of(2, 1, 9)));
    assertTrue(SemVer.of(100, 0, 0).isGreaterThan(SemVer.of(1, 0, 0)));
    assertTrue(SemVer.of(21, 0, 0).isGreaterThan(SemVer.of(9, 0, 0)));
    assertEquals(
        SemVer.parse("0.2.17-SNAPSHOT.2021-07-23").get().preReleaseVersion(),
        "SNAPSHOT.2021-07-23");

    assertTrue(
        SemVer.parse("0.2.17-SNAPSHOT.2021-07-23")
            .get()
            .isGreaterThanOrEqual(SemVer.of(0, 2, 17, "SNAPSHOT")));

    var v5 = SemVer.parse("0.2.17-SNAPSHOT.2021-07-23").get();
    var v6 = SemVer.parse("0.2.17-SNAPSHOT").get();
    assertTrue(v5.isGreaterThanOrEqual(v6));
  }

  @Test
  public void testInvalidVersion() throws Exception {
    var v1 = SemVer.parse("1.001.0");
    assertTrue(v1.isFailure());
  }
}
