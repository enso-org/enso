package org.enso.table.data.column.operation.masks;

import java.util.stream.LongStream;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class IndexMapperReversedTests {
  private IndexMapper makeBase() {
    return new IndexMapper.Reversed(38, 123);
  }

  @Test
  public void returnsSize() {
    var base = makeBase();
    Assertions.assertEquals(123, base.size());
  }

  @Test
  public void mapsIndex() {
    var base = makeBase();
    for (long i = 0; i < 123; i++) {
      Assertions.assertEquals(38 + 123 - 1 - i, base.map(i));
    }
  }

  @Test
  public void rejectsNegativeSize() {
    Assertions.assertThrowsExactly(
        IllegalArgumentException.class, () -> new IndexMapper.SingleSlice(-1, 123));
    Assertions.assertThrowsExactly(
        IllegalArgumentException.class, () -> new IndexMapper.SingleSlice(13, -5));
  }

  @Test
  public void failsOnOutOfRangeIndex() {
    var base = makeBase();
    Assertions.assertThrowsExactly(IndexOutOfBoundsException.class, () -> base.map(-1));
    Assertions.assertThrowsExactly(IndexOutOfBoundsException.class, () -> base.map(123));
  }

  @Test
  public void mergeConstant() {
    var base = makeBase();

    Assertions.assertThrowsExactly(
        IllegalArgumentException.class,
        () -> {
          base.merge(new IndexMapper.Constant(100));
        });
  }

  @Test
  public void mergeSliceAtStart() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 100));
    Assertions.assertInstanceOf(IndexMapper.Reversed.class, sliced);
    Assertions.assertEquals(100, sliced.size());
    Assertions.assertEquals(61, ((IndexMapper.Reversed) sliced).start);
  }

  @Test
  public void mergeSliceAtStartOverflowing() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 300));
    Assertions.assertInstanceOf(IndexMapper.Reversed.class, sliced);
    Assertions.assertEquals(123, sliced.size());
    Assertions.assertEquals(38, ((IndexMapper.Reversed) sliced).start);
  }

  @Test
  public void mergeSliceHalfwayUp() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(50, 100));
    Assertions.assertInstanceOf(IndexMapper.Reversed.class, sliced);
    Assertions.assertEquals(73, sliced.size());
    Assertions.assertEquals(88, ((IndexMapper.Reversed) sliced).start);
  }

  @Test
  public void failsOnOutOfRangeSlice() {
    var base = makeBase();

    Assertions.assertThrowsExactly(
        IndexOutOfBoundsException.class, () -> base.merge(new IndexMapper.SingleSlice(243, 100)));
  }

  @Test
  public void mergeReverseWholeSlice() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(0, 123));
    Assertions.assertInstanceOf(IndexMapper.SingleSlice.class, sliced);
    Assertions.assertEquals(123, sliced.size());
    Assertions.assertEquals(38, ((IndexMapper.SingleSlice) sliced).start);
  }

  @Test
  public void mergeReverseAtStart() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(0, 50));
    Assertions.assertInstanceOf(IndexMapper.SingleSlice.class, sliced);
    Assertions.assertEquals(50, sliced.size());
    Assertions.assertEquals(111, ((IndexMapper.SingleSlice) sliced).start);
  }

  @Test
  public void mergeReverseOverflowing() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(80, 100));
    Assertions.assertInstanceOf(IndexMapper.SingleSlice.class, sliced);
    Assertions.assertEquals(43, sliced.size());
    Assertions.assertEquals(118, ((IndexMapper.SingleSlice) sliced).start);
  }

  @Test
  public void failsOnOutOfRangeReverse() {
    var base = makeBase();

    Assertions.assertThrowsExactly(
        IndexOutOfBoundsException.class, () -> base.merge(new IndexMapper.Reversed(243, 100)));
  }

  @Test
  public void mergeArrayMapping() {
    var base = makeBase();
    var mapping = new long[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

    var sliced = base.merge(new IndexMapper.ArrayMapping(mapping));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(10, sliced.size());
    Assertions.assertArrayEquals(
        new long[] {160, 159, 158, 157, 156, 155, 154, 153, 152, 151},
        ((IndexMapper.ArrayMapping) sliced).mapping);
  }

  @Test
  public void mergeArrayMappingWithNotFound() {
    var base = makeBase();

    var sliced =
        base.merge(
            new IndexMapper.ArrayMapping(new long[] {0, 1, 2, IndexMapper.NOT_FOUND_INDEX, 4}));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(5, sliced.size());
    Assertions.assertArrayEquals(
        new long[] {160, 159, 158, IndexMapper.NOT_FOUND_INDEX, 156},
        ((IndexMapper.ArrayMapping) sliced).mapping);
  }

  @Test
  public void mergeEmptyArrayMapping() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.ArrayMapping(new long[0]));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(0, sliced.size());
  }

  @Test
  public void mergeLargeArrayMapping() {
    var base = makeBase();
    var sliced =
        base.merge(
            new IndexMapper.ArrayMapping(
                LongStream.range(0, 10000).map(idx -> idx % 123).toArray()));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(10000, sliced.size());
  }

  @Test
  public void failsOnOutOfRangeArray() {
    var base = makeBase();

    Assertions.assertThrowsExactly(
        IndexOutOfBoundsException.class,
        () ->
            base.merge(
                new IndexMapper.ArrayMapping(
                    LongStream.range(0, 10000).map(idx -> idx + 123).toArray())));
  }
}
