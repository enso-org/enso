package org.enso.common;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Properties;
import org.junit.Test;

public class EnsoSourcesTest {
  @Test
  public void verifyEngineCommonSourcesFile() throws Exception {
    var url = EnsoSourcesTest.class.getProtectionDomain().getCodeSource().getLocation();
    var where = new File(url.toURI());
    var engineCommonDir = findParentDir(where, ".enso-sources");
    var mainSrc = loadProps(file(engineCommonDir, ".enso-sources-classes"));
    var mainInput = mainSrc.get("input");
    assertEquals(
        "Points to src/main/java",
        file(engineCommonDir, "src", "main", "java").toString(),
        mainInput);
    var testSrc = loadProps(file(engineCommonDir, ".enso-sources-test-classes"));
    var testInput = testSrc.get("input");
    assertEquals(
        "Points to src/test/java",
        file(engineCommonDir, "src", "test", "java").toString(),
        testInput);
  }

  private Properties loadProps(File src) throws IOException {
    var props = new Properties();
    props.load(new FileInputStream(src));
    return props;
  }

  private static File findParentDir(File root, String... names) {
    for (var d = root; d != null; d = d.getParentFile()) {
      var f = file(d, names);
      if (f.exists()) {
        return d;
      }
    }
    throw new AssertionError(
        "Cannot find " + Arrays.toString(names) + " in parent directories of " + root);
  }

  private static File file(File root, String... children) {
    for (var ch : children) {
      root = new File(root, ch);
    }
    return root;
  }
}
