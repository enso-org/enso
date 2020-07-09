package org.enso.interpreter.runtime.data;

public final class SingletonState {
  private final Object key;
  private final Object value;

  public SingletonState(Object key, Object value) {
    this.key = key;
    this.value = value;
  }

  public Object getKey() {
    return key;
  }

  public Object getValue() {
    return value;
  }
}
