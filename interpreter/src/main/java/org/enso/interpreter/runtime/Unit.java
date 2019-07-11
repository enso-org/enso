package org.enso.interpreter.runtime;

public class Unit {

  private static final Unit singleton = new Unit();

  private Unit() {}

  public static Unit instance() {
    return Unit.singleton;
  }
}
