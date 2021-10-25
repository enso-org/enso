package org.enso.interpreter.runtime.state.data;

/** An object representing a single key-value pairing. */
public final class SingletonMap {
  private final Object key;
  private final Object value;

  /**
   * Creates a new key-value pair.
   *
   * @param key the key of this pair
   * @param value the value of this pair
   */
  public SingletonMap(Object key, Object value) {
    this.key = key;
    this.value = value;
  }

  /** @return the key of this pair */
  public Object getKey() {
    return key;
  }

  /** @return the value of this pair */
  public Object getValue() {
    return value;
  }
}
