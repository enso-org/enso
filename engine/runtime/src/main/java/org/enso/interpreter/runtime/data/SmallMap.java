package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;

public class SmallMap implements TruffleObject {
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] keys;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] values;
  private static final SmallMap EMPTY = new SmallMap(new Object[0], new Object[0]);
  private static Object[] len1Keys;

  public static final int NOT_FOUND = -1;

  public static SmallMap empty() {
    return EMPTY;
  }

  private SmallMap(Object[] keys, Object[] values) {
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

  private Object[] arrayPrepend(Object newItem, Object[] items) {
    Object[] newItems = new Object[items.length + 1];
    System.arraycopy(items, 0, newItems, 0, items.length);
    newItems[items.length] = newItem;
    return newItems;
  }

  public final SmallMap insert(Object key, Object value) {
    Object[] newKeys = arrayPrepend(key, keys);
    Object[] newValues = arrayPrepend(value, values);
    return new SmallMap(newKeys, newValues);
  }

  public final SmallMap insert(Object keys[], Object value) {
    Object[] newValues = arrayPrepend(value, values);
    return new SmallMap(keys, newValues);
  }

  public final SmallMap set(int key, Object value) {
    Object[] newValues = new Object[values.length];
    System.arraycopy(values, 0, newValues, 0, values.length);
    newValues[key] = value;
    return new SmallMap(keys, newValues);
  }

  public Object[] getKeys() {
    return keys;
  }

  public Object get(int idx) {
    return values[idx];
  }

  public Object[] getSchemaAfterInsert(Object key) {
    if (len1Keys != null) {
      return len1Keys;
    }
    len1Keys = arrayPrepend(key, keys);
    return len1Keys;
  }
}
