package org.enso.table.util;

import java.util.BitSet;
import org.graalvm.polyglot.Context;

public class BitSets {
  /**
   * An utility to copy a part of one bitset onto another, with a possible destination offset.
   *
   * <p>Unfortunately BitSet does not provide a fast way to do this. We could try to implement
   * something on our own that would operate on whole longs instead of bit by bit.
   */
  public static void copy(BitSet source, BitSet destination, int destinationOffset, int length) {
    if (destinationOffset == 0) {
      destination.clear(0, length);
      destination.or(source.get(0, length));
      return;
    }

    Context context = Context.getCurrent();
    for (int i = 0; i < length; i++) {
      if (source.get(i)) {
        destination.set(destinationOffset + i);
      }

      context.safepoint();
    }
  }

  public static BitSet makeDuplicate(BitSet source) {
    BitSet result = new BitSet();
    result.or(source);
    return result;
  }
}
