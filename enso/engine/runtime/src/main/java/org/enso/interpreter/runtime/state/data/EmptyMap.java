package org.enso.interpreter.runtime.state.data;

import com.oracle.truffle.api.interop.TruffleObject;

/** A dummy type, denoting an empty map structure. */
public final class EmptyMap implements TruffleObject {
  private static final EmptyMap INSTANCE = new EmptyMap();

  private EmptyMap() {}

  /** @return an instance of empty map. */
  public static EmptyMap create() {
    return INSTANCE;
  }
}
