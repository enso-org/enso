package org.enso.table.data.column.operation.map.numeric.isin;

import java.util.HashSet;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.graalvm.polyglot.Context;

public class DoubleIsInOp extends SpecializedIsInOp<Double, DoubleStorage> {
  @Override
  protected CompactRepresentation<Double> prepareList(List<?> list) {
    Context context = Context.getCurrent();
    HashSet<Double> set = new HashSet<>();
    boolean hasNulls = false;
    for (Object o : list) {
      hasNulls |= o == null;
      Double x = NumericConverter.tryConvertingToDouble(o);
      if (x != null) {
        set.add(x);
      }

      context.safepoint();
    }
    return new CompactRepresentation<>(set, hasNulls);
  }
}
