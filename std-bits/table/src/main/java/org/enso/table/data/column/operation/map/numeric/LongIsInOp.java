package org.enso.table.data.column.operation.map.numeric;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.storage.LongStorage;

import java.util.HashSet;
import java.util.List;

public class LongIsInOp extends SpecializedIsInOp<Long, LongStorage> {
  @Override
  protected CompactRepresentation<Long> prepareList(List<?> list) {
    HashSet<Long> set = new HashSet<>();
    boolean hasNulls = false;
    for (Object o : list) {
      hasNulls |= o == null;
      Long x = NumericConverter.tryConvertingToLong(o);
      if (x != null) {
        set.add(x);
      }
    }
    return new CompactRepresentation<>(set, hasNulls);
  }
}
