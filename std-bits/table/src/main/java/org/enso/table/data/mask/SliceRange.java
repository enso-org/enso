package org.enso.table.data.mask;

import java.util.List;

public interface SliceRange {
  int start();

  int end();

  static int totalLength(List<SliceRange> ranges) {
    int total = 0;
    for (SliceRange range : ranges) {
      total += range.end() - range.start();
    }
    return total;
  }
}
