package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.TruffleObject;

public class EmptyState implements TruffleObject {
  private static final EmptyState INSTANCE = new EmptyState();

  private EmptyState() {}

  public static EmptyState create() {
    return INSTANCE;
  }
}
