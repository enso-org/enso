package org.enso.table.data.column.builder;

import java.math.BigDecimal;
import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.error.ValueTypeMismatchException;
import org.graalvm.polyglot.Context;

/** A builder for BigDecimal columns. */
public final class BigDecimalBuilder extends TypedBuilder<BigDecimal> {
  /** Creates a new empty BigDecimal storage with the specified size. */
  public static ColumnStorage<BigDecimal> makeEmpty(long size) {
    int intSize = Builder.checkSize(size);
    return new BigDecimalStorage(new BigDecimal[intSize]);
  }

  BigDecimalBuilder(int size) {
    super(BigDecimalType.INSTANCE, new BigDecimal[size]);
  }

  @Override
  public void append(Object o) {
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
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof BigDecimal || NumericConverter.isCoercibleToDouble(o);
  }

  @Override
  protected ColumnStorage<BigDecimal> doSeal() {
    return new BigDecimalStorage(data);
  }

  static Builder retypeFromLongBuilder(LongBuilder longBuilder) {
    var res = new BigDecimalBuilder(longBuilder.data.length);
    int n = longBuilder.currentSize;
    Context context = Context.getCurrent();
    for (int i = 0; i < n; i++) {
      res.append(BigDecimal.valueOf(longBuilder.data[i]));
      context.safepoint();
    }
    return res;
  }
}
