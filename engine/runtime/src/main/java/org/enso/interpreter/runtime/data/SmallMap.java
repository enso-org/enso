package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;

public final class SmallMap implements TruffleObject {
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] keys;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] values;
  private static final SmallMap EMPTY = new SmallMap(new Object[0], new Object[0]);

  public static final int NOT_FOUND = -1;

  public static SmallMap empty() {
    return EMPTY;
  }

  public SmallMap(Object[] keys, Object[] values) {
    this.keys = keys;
    this.values = values;
  }

  @CompilerDirectives.TruffleBoundary
  public int indexOf(Object key) {
    for (int i = 0; i < keys.length; i++) {
      if (key == keys[i]) {
        return i;
      }
    }
    return NOT_FOUND;
  }

  public Object[] getKeys() {
    return keys;
  }

  public Object get(int idx) {
    return values[idx];
  }

  public Object[] getValues() {
    return values;
  }
}
