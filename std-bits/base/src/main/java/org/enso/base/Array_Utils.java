package org.enso.base;

import java.util.ArrayList;
import java.util.List;

public class Array_Utils {
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

  /** A temporary workaround to be able to efficiently append an array to `ArrayList`. */
  public static <T> void appendToArrayList(ArrayList<T> builder, List<T> list) {
    builder.addAll(list);
  }
}
