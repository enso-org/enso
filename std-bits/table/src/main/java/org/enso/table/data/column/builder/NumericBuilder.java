package org.enso.table.data.column.builder;

import java.util.Arrays;
import java.util.BitSet;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.problems.ProblemAggregator;

/** A common base for numeric builders. */
public abstract class NumericBuilder extends TypedBuilder {
  protected BitSet isMissing;
  protected long[] data;
  protected int currentSize;

  NumericBuilder(BitSet isMissing, long[] data, int currentSize) {
    this.isMissing = isMissing;
    this.data = data;
    this.currentSize = currentSize;
  }

  /**
   * Creates a {@link DoubleBuilder} that should be used to create columns of boolean type and are
   * not expected to be retyped.
   */
  public static DoubleBuilder createDoubleBuilder(int size, ProblemAggregator problemAggregator) {
    return new DoubleBuilder(new BitSet(), new long[size], 0, problemAggregator);
  }

  /** Creates a {@link DoubleBuilder} that may be retyped to Mixed type. */
  public static DoubleBuilder createInferringDoubleBuilder(
      int size, ProblemAggregator problemAggregator) {
    return new InferringDoubleBuilder(new BitSet(), new long[size], 0, problemAggregator);
  }

  public static LongBuilder createLongBuilder(
      int size, IntegerType type, ProblemAggregator problemAggregator) {
    return LongBuilder.make(size, type, problemAggregator);
  }

  @Override
  public void appendNulls(int count) {
    isMissing.set(currentSize, currentSize + count);
    currentSize += count;
  }

  protected void ensureFreeSpaceFor(int additionalSize) {
    if (currentSize + additionalSize > data.length) {
      resize(currentSize + additionalSize);
    }
  }

  @Override
  public void append(Object o) {
    if (currentSize >= data.length) {
      grow();
    }
    appendNoGrow(o);
  }

  /**
   * Append a new item in raw form to this builder, assuming that it has enough allocated space.
   *
   * <p>This function should only be used when it is guaranteed that the builder has enough
   * capacity, for example if it was initialized with an initial capacity known up-front.
   *
   * @param rawData the raw encoding of the item, for long numbers just the number and for doubles,
   *     its long bytes
   */
  public void appendRawNoGrow(long rawData) {
    data[currentSize++] = rawData;
  }

  @Override
  public int getCurrentSize() {
    return currentSize;
  }

  /**
   * Grows the underlying array.
   *
   * <p>The method grows the array by 50% by default to amortize the re-allocation time over
   * appends. It tries to keep the invariant that after calling `grow` the array has at least one
   * free slot.
   */
  protected void grow() {
    int desiredCapacity = 3;
    if (data.length > 1) {
      desiredCapacity = (data.length * 3 / 2);
    }

    // It is possible for the `currentSize` to grow arbitrarily larger than
    // the capacity, because when nulls are being added the array is not
    // resized, only the counter is incremented. Thus, we need to ensure
    // that we have allocated enough space for at least one element.
    if (currentSize >= desiredCapacity) {
      desiredCapacity = currentSize + 1;
    }

    resize(desiredCapacity);
  }

  protected void resize(int desiredCapacity) {
    this.data = Arrays.copyOf(data, desiredCapacity);
  }
}
