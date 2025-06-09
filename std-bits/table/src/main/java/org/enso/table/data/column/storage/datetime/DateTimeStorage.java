package org.enso.table.data.column.storage.datetime;

import java.time.Duration;
import java.time.ZonedDateTime;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.GenericBinaryObjectMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.storage.SpecializedStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.DateTimeType;

public final class DateTimeStorage extends SpecializedStorage<ZonedDateTime> {
  private static final MapOperationStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> OPS =
      buildOps();

  private static MapOperationStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> buildOps() {
    MapOperationStorage<ZonedDateTime, SpecializedStorage<ZonedDateTime>> t =
        new MapOperationStorage<>();
    t.add(
        new GenericBinaryObjectMapOperation<
            ZonedDateTime, SpecializedStorage<ZonedDateTime>, Duration>(
            Maps.SUB, ZonedDateTime.class, DateTimeStorage.class) {
          @Override
          protected Builder createOutputBuilder(long size) {
            return Builder.getObjectBuilder(size);
          }

          @Override
          protected Duration run(ZonedDateTime value, ZonedDateTime other) {
            return Duration.between(other, value);
          }
        });
    return t;
  }

  /**
   * @param data the underlying data
   */
  public DateTimeStorage(ZonedDateTime[] data) {
    super(DateTimeType.INSTANCE, data);
  }

  @Override
  protected Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemAggregator problemAggregator) {
    return OPS.runBinaryMap(name, this, argument, problemAggregator);
  }

  @Override
  protected Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemAggregator problemAggregator) {
    return OPS.runZip(name, this, argument, problemAggregator);
  }

  @Override
  protected SpecializedStorage<ZonedDateTime> newInstance(ZonedDateTime[] data) {
    return new DateTimeStorage(data);
  }

  @Override
  protected ZonedDateTime[] newUnderlyingArray(int size) {
    return new ZonedDateTime[size];
  }
}
