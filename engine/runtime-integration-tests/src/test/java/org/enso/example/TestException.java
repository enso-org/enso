package org.enso.example;

public class TestException extends Exception {
  public TestException() {}

  public static void throwMe() throws Exception {
    throw new TestException();
  }

  public static void throwSubtype() throws Exception {
    throw new SubException();
  }

  private static final class SubException extends TestException {}
}
