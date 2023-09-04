package org.enso.table.data.column.builder;

import java.time.ZonedDateTime;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.datetime.DateTimeStorage;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.error.ValueTypeMismatchException;

/** A builder for string columns. */
public class DateTimeBuilder extends TypedBuilderImpl<ZonedDateTime> {
  @Override
  protected ZonedDateTime[] newArray(int size) {
    return new ZonedDateTime[size];
  }

  public DateTimeBuilder(int size) {
    super(size);
  }

  @Override
  public StorageType getType() {
    return DateTimeType.INSTANCE;
  }

  @Override
  public void appendNoGrow(Object o) {
    try {
      data[currentSize++] = (ZonedDateTime) o;
    } catch (ClassCastException e) {
      throw new ValueTypeMismatchException(getType(), o);
    }
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof ZonedDateTime;
  }

  @Override
  protected Storage<ZonedDateTime> doSeal() {
    return new DateTimeStorage(data, currentSize);
  }
}
