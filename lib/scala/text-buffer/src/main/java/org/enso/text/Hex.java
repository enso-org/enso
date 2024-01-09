package org.enso.text;

/**
 * A utility class that serves as a replacement of some methods from {@code org.bouncycastle}, so
 * that we don't have to depend on it.
 */
public final class Hex {
  /** Converts the byte array to its hexadecimal String representation. */
  public static String toHexString(byte[] bytes) {
    char[] out = new char[bytes.length * 2];
    for (int i = 0; i < bytes.length; i++) {
      int v = bytes[i] & 0xFF;
      out[i * 2] = Character.forDigit(v >>> 4, 16);
      out[i * 2 + 1] = Character.forDigit(v & 0x0F, 16);
    }
    return new String(out);
  }
}
