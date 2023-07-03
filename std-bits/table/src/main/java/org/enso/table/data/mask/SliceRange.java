package org.enso.table.data.mask;

import java.util.List;
import org.graalvm.polyglot.Context;

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
