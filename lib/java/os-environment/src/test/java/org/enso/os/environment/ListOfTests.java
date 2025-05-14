package org.enso.os.environment;

import java.util.List;

public final class ListOfTests {
  private ListOfTests() {}

  public static final List<String> TEST_CLASSES =
      List.of(
          "org.enso.os.environment.PlatformTest",
          "org.enso.os.environment.RandomUtilsTest",
          "org.enso.os.environment.jni.LoadClassTest",
          "org.enso.os.environment.chdir.TestChangeDirectory",
          "org.enso.os.environment.directories.DirectoriesTest",
          "org.enso.os.environment.trash.TrashBinTest");
}
