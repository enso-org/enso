package org.enso.os.environment;

import java.util.List;

public final class ListOfTests {
  private ListOfTests() {}

  public static final List<String> TEST_CLASSES =
      List.of("org.enso.os.environment.chdir.TestChangeDirectory");
}
