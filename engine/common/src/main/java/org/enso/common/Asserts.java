package org.enso.common;

public final class Asserts {
  private Asserts() {}

  public static void assertInJvm(boolean check) {
    assert check;
  }

  public static void assertInJvm(boolean check, String msg) {
    assert check : msg;
  }
}
