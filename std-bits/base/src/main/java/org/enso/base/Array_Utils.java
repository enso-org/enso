package org.enso.base;

public final class Array_Utils {
  private Array_Utils() {}

  /**
   * This function forces the polyglot conversion of an Enso array into a `byte[]`. This allows for
   * asserting that it is a valid `byte[]`.
   *
   * @param input the converted array.
   * @return the `input` unchanged.
   */
  public static byte[] ensureByteArray(byte[] input) {
    return input;
  }
}
