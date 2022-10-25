package org.enso.table.data.column.operation.map.numeric;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.Storage;

import java.util.HashSet;
import java.util.List;

public class DoubleIsInOp extends SpecializedIsInOp<Double, DoubleStorage> {
  @Override
  protected CompactRepresentation<Double> prepareList(List<?> list) {
    HashSet<Double> set = new HashSet<>();
    boolean hasNulls = false;
    for (Object o : list) {
      hasNulls |= o == null;
      Double x = NumericConverter.tryConvertingToDouble(o);
      if (x != null) {
        set.add(x);
      }
    }
    return new CompactRepresentation<>(set, hasNulls);
  }
}
