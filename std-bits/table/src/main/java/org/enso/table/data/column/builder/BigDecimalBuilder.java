package org.enso.table.data.column.builder;

import java.math.BigDecimal;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.TypedStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.error.ValueTypeMismatchException;
import org.graalvm.polyglot.Context;

/** A builder for BigDecimal columns. */
final class BigDecimalBuilder extends TypedBuilder<BigDecimal> {
  BigDecimalBuilder(int size) {
    super(BigDecimalType.INSTANCE, new BigDecimal[size]);
  }

  @Override
  public BigDecimalBuilder append(Object o) {
    ensureSpaceToAppend();
    if (o == null) {
      appendNulls(1);
    } else {
      try {
        data[currentSize++] = NumericConverter.coerceToBigDecimal(o);
      } catch (UnsupportedOperationException e) {
        throw new ValueTypeMismatchException(getType(), o);
      }
    }
    return this;
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof BigDecimal || NumericConverter.isCoercibleToDouble(o);
  }

  @Override
  protected ColumnStorage<BigDecimal> doSeal() {
    return new TypedStorage<>(BigDecimalType.INSTANCE, data);
  }

  static Builder retypeFromLongBuilder(BuilderForLong longBuilder) {
    var res = Builder.getForBigDecimal(longBuilder.getCurrentCapacity());
    long n = longBuilder.getCurrentSize();
    Context context = Context.getCurrent();
    for (long i = 0; i < n; i++) {
      res.append(longBuilder.isNothing(i) ? null : BigDecimal.valueOf(longBuilder.getLong(i)));
      context.safepoint();
    }
    return res;
  }
}
