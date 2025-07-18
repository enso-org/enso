package org.enso.example;

import org.graalvm.polyglot.PolyglotException;

public class ToString {

  private ToString() {}

  public static interface Fooable {
    public long foo();
  }

  public static String callFoo(Fooable f) {
    try {
      long x = f.foo();
      return "Fooable.foo() = " + x;
    } catch (Throwable t) {
      throw t;
    }
  }

  public static String showObject(Object obj) {
    return "obj.toString() = " + obj.toString();
  }

  public static String callFooAndShow(Fooable f) {
    var x = f.foo();
    try {
      var s = f.toString();
      return "{" + s + "}.foo() = " + x;
    } catch (PolyglotException ex) {
      return "Ex: " + ex.getMessage() + ", but foo() = " + x;
    }
  }
}
