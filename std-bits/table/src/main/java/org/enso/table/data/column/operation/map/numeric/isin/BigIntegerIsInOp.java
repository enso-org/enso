package org.enso.table.data.column.operation.map.numeric.isin;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.List;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.storage.Storage;
import org.graalvm.polyglot.Context;

public class BigIntegerIsInOp<S extends Storage<BigInteger>>
    extends SpecializedIsInOp<BigInteger, S> {
  @Override
  protected CompactRepresentation<BigInteger> prepareList(List<?> list) {
    Context context = Context.getCurrent();
    HashSet<BigInteger> set = new HashSet<>();
    boolean hasNulls = false;
    for (Object o : list) {
      hasNulls |= o == null;

      if (o instanceof BigInteger bigInteger) {
        set.add(bigInteger);
      } else {
        Long x = NumericConverter.tryConvertingToLong(o);
        if (x != null) {
          set.add(BigInteger.valueOf(x));
        }
      }

      context.safepoint();
    }
    return new CompactRepresentation<>(set, hasNulls);
  }
}
