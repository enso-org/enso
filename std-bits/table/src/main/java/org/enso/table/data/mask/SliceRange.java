package org.enso.table.data.mask;

import java.util.List;

public interface SliceRange {
  long start();

  long end();

  static long totalLength(List<SliceRange> ranges) {
    long total = 0;
    for (SliceRange range : ranges) {
      total += range.end() - range.start();
    }
    return total;
  }

  static long[] createMask(List<SliceRange> ranges) {
    long size = totalLength(ranges);
    if (size > Integer.MAX_VALUE) {
      throw new IllegalArgumentException(
          "The total length of the slice ranges exceeds Integer.MAX_VALUE.");
    }

    long[] mask = new long[(int) size];
    int index = 0;
    for (SliceRange range : ranges) {
      for (long i = range.start(); i < range.end(); i++) {
        mask[index++] = i;
      }
    }
    return mask;
  }
}
