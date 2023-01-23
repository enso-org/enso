package org.enso.table.data.column.storage;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.DateTimeBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.operation.map.datetime.DateTimeIsInOp;

import java.time.ZonedDateTime;

public final class DateTimeStorage extends SpecializedStorage<ZonedDateTime> {
  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public DateTimeStorage(ZonedDateTime[] data, int size) {
    super(data, size, ops);
  }

  private static final MapOpStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> ops =
      buildOps();

  private static MapOpStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> buildOps() {
    MapOpStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> t =
        ObjectStorage.buildObjectOps();
    t.add(new DateTimeIsInOp<>(ZonedDateTime.class));
    return t;
  }

  @Override
  protected SpecializedStorage<ZonedDateTime> newInstance(ZonedDateTime[] data, int size) {
    return new DateTimeStorage(data, size);
  }

  @Override
  protected ZonedDateTime[] newUnderlyingArray(int size) {
    return new ZonedDateTime[size];
  }

  @Override
  public int getType() {
    return Type.DATE_TIME;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new DateTimeBuilder(capacity);
  }
}
