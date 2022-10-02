package org.enso.table.data.column.builder.object;

import org.enso.table.data.column.storage.DateTimeStorage;
import org.enso.table.data.column.storage.Storage;

import java.time.ZonedDateTime;

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
  public int getType() {
    return Storage.Type.DATE_TIME;
  }

  @Override
  public void appendNoGrow(Object o) {
    data[currentSize++] = (ZonedDateTime) o;
  }

  @Override
  public boolean accepts(Object o) {
    return o instanceof ZonedDateTime;
  }

  @Override
  public Storage seal() {
    return new DateTimeStorage(data, currentSize);
  }
}
