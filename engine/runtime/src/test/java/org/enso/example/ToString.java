package org.enso.example;

public class ToString {

  private ToString() {}

  public static interface Fooable {
    public long foo();
  }

  public static String callFoo(Fooable f) {
    long x = f.foo();
    return "Fooable.foo() = " + x;
  }

  public static String showObject(Object obj) {
    return "obj.toString() = " + obj.toString();
  }

  public static String callFooAndShow(Fooable f) {
    long x = f.foo();
    return "{" + f.toString() + "}.foo() = " + x;
  }
}
