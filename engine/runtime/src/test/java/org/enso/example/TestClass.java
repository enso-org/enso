package org.enso.example;

import java.util.function.Function;

/** A class used for testing Java Interop from Enso code */
public class TestClass {

  private final Function<Long, Long> function;

  public TestClass(Function<Long, Long> function) {
    this.function = function;
  }

  public TestClass() {
    this(x -> x);
  }

  public static long add(long a, long b) {
    return a + b;
  }

  public long callFunctionAndIncrement(long argument) {
    return function.apply(argument) + 1;
  }

  public void method1() {}

  public void method2() {}
}
