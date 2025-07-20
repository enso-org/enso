package org.enso.example;

public final class TestException extends Exception {
  public TestException() {}

  public static void throwMe() throws Exception {
    throw new TestException();
  }
}
