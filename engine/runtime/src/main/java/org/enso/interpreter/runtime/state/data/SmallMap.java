package org.enso.interpreter.runtime.state.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;

/**
 * Represents an arbitrary-size map-like structure. It is low-level and only works well for small
 * numbers of keys.
 */
public final class SmallMap implements TruffleObject {
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] keys;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] values;
  private static final SmallMap EMPTY = new SmallMap(new Object[0], new Object[0]);

  public static final int NOT_FOUND = -1;

  /** @return an empty instance of this class */
  public static SmallMap empty() {
    return EMPTY;
  }

  /**
   * Creates a map with given keys and values.
   *
   * @param keys the keys of this map.
   * @param values the values of this map. Must have the same length as {@code keys}.
   */
  public SmallMap(Object[] keys, Object[] values) {
    this.keys = keys;
    this.values = values;
  }

  /**
   * Returns the index of a given key in the keys array. Returns {@code NOT_FOUND} if the key is
   * missing.
   *
   * @param key the key to lookup
   * @return the key's index or {@code NOT_FOUND}
   */
  @CompilerDirectives.TruffleBoundary
  public int indexOf(Object key) {
    for (int i = 0; i < keys.length; i++) {
      if (key == keys[i]) {
        return i;
      }
    }
    return NOT_FOUND;
  }

  /** @return the keys in this map. */
  public Object[] getKeys() {
    return keys;
  }

  /** @return the values in this map. */
  public Object[] getValues() {
    return values;
  }
}
