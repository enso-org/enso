package org.enso.table_test_helpers;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;

/**
 * A helper class used in the Upload_Spec test to purposefully interrupt a table upload in the
 * middle of it by throwing an exception. It is used to test the transactionality of the upload.
 */
public class ExplodingStorage extends Storage<Long> {
  private final long[] array;
  private final long explodingIndex;

  public ExplodingStorage(long[] array, long explodingIndex) {
    this.array = array;
    this.explodingIndex = explodingIndex;
  }

  private void checkIndex(long idx) {
    if (idx == explodingIndex) {
      throw new ExplodingStoragePayload();
    }
  }

  @Override
  public int size() {
    return array.length;
  }

  @Override
  public int countMissing() {
    return 0;
  }

  @Override
  public StorageType getType() {
    return IntegerType.INT_64;
  }

  @Override
  public boolean isNa(long idx) {
    checkIndex(idx);
    return false;
  }

  public long getItem(int idx) {
    checkIndex(idx);
    return array[idx];
  }

  @Override
  public Long getItemBoxed(int idx) {
    return getItem(idx);
  }

  @Override
  public boolean isUnaryOpVectorized(String name) {
    return false;
  }

  @Override
  public boolean isBinaryOpVectorized(String name) {
    return false;
  }

  @Override
  public Storage<?> runVectorizedUnaryMap(String name, MapOperationProblemBuilder problemBuilder) {
    return null;
  }

  @Override
  public Storage<?> runVectorizedBinaryMap(
      String name, Object argument, MapOperationProblemBuilder problemBuilder) {
    return null;
  }

  @Override
  public Storage<?> runVectorizedZip(
      String name, Storage<?> argument, MapOperationProblemBuilder problemBuilder) {
    return null;
  }

  @Override
  public Storage<Long> mask(BitSet mask, int cardinality) {
    return null;
  }

  @Override
  public Storage<Long> applyMask(OrderMask mask) {
    return null;
  }

  @Override
  public Storage<Long> countMask(int[] counts, int total) {
    return null;
  }

  @Override
  public Storage<Long> slice(int offset, int limit) {
    return null;
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return null;
  }

  @Override
  public Storage<Long> slice(List<SliceRange> ranges) {
    return null;
  }
}
