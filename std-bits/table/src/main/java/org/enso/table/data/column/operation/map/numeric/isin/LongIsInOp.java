package org.enso.table.data.column.operation.map.numeric.isin;

import java.util.HashSet;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.graalvm.polyglot.Context;

public class LongIsInOp extends SpecializedIsInOp<Long, AbstractLongStorage> {
  @Override
  protected CompactRepresentation<Long> prepareList(List<?> list) {
    Context context = Context.getCurrent();
    HashSet<Long> set = new HashSet<>();
    boolean hasNulls = false;
    for (Object o : list) {
      hasNulls |= o == null;
      Long x = NumericConverter.tryConvertingToLong(o);
      if (x != null) {
        set.add(x);
      }

      context.safepoint();
    }
    return new CompactRepresentation<>(set, hasNulls);
  }
}
