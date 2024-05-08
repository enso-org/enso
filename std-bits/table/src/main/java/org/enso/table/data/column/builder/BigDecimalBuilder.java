package org.enso.table.data.column.builder;

import java.math.BigDecimal;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.numeric.BigDecimalStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for BigDecimal columns. */
public class BigDecimalBuilder extends TypedBuilderImpl<BigDecimal> {
  @Override
  protected BigDecimal[] newArray(int size) {
    return new BigDecimal[size];
  }

  public BigDecimalBuilder(int size) {
    super(size);
  }

  @Override
  public StorageType getType() {
    return BigDecimalType.INSTANCE;
  }

  @Override
  public void appendNoGrow(Object o) {
    try {
      data[currentSize++] = (BigDecimal) o;
    } catch (ClassCastException e) {
      throw new ValueTypeMismatchException(getType(), o);
    }
  }

  @Override
  public void append(Object o) {
    appendNoGrow(o);
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof BigDecimal;
  }

  @Override
  protected Storage<BigDecimal> doSeal() {
    return new BigDecimalStorage(data, currentSize);
  }
}
