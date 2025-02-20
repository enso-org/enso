package org.enso.compiler.pass;

import scala.collection.immutable.List;

public final class PassManagerTestUtils {
  private PassManagerTestUtils() {}

  public static List<PassGroup> getPasses(PassManager passManager) {
    return passManager.passes();
  }
}
