package org.enso.table.data.mask;

import org.graalvm.polyglot.Context;

import java.util.List;

public interface SliceRange {
  int start();

  int end();

  static int totalLength(List<SliceRange> ranges) {
    Context context = Context.getCurrent();
    int total = 0;
    for (SliceRange range : ranges) {
      total += range.end() - range.start();
      context.safepoint();
    }
    return total;
  }
}
