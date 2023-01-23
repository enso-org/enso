package org.enso.table.util;

import java.util.BitSet;

public class BitSets {
  /**
   * An utility to copy a part of one bitset onto another, with a possible destination offset.
   *
   * <p>Unfortunately BitSet does not provide a fast way to do this. We could try to implement
   * something on our own that would operate on whole longs instead of bit by bit.
   */
  public static void copy(BitSet source, BitSet destination, int destinationOffset, int length) {
    for (int i = 0; i < length; i++) {
      if (source.get(i)) {
        destination.set(destinationOffset + i);
      }
    }
  }
}
