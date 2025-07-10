package org.enso.table.data.column.operation.masks;

import java.util.Arrays;
import java.util.stream.IntStream;
import java.util.stream.LongStream;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class IndexMapperArrayTests {
  private final long[] rawMapping =
      new long[] {
        32,
        43,
        56,
        IndexMapper.NOT_FOUND_INDEX,
        53,
        12,
        15,
        13,
        8,
        9,
        23,
        42,
        9,
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8,
        9
      };

  private IndexMapper makeBase() {
    return new IndexMapper.ArrayMapping(rawMapping);
  }

  @Test
  public void returnsSize() {
    var base = makeBase();
    Assertions.assertEquals(rawMapping.length, base.size());
  }

  @Test
  public void mapsIndex() {
    var base = makeBase();
    for (int i = 0; i < rawMapping.length; i++) {
      Assertions.assertEquals(rawMapping[i], base.map(i));
    }
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

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 5));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(5, sliced.size());
    Assertions.assertArrayEquals(
        Arrays.stream(rawMapping, 0, 5).toArray(), ((IndexMapper.ArrayMapping) sliced).mapping);
  }

  @Test
  public void mergeSliceAtStartOverflowing() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 300));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(rawMapping.length, sliced.size());
    Assertions.assertArrayEquals(rawMapping, ((IndexMapper.ArrayMapping) sliced).mapping);
  }

  @Test
  public void mergeSliceHalfwayUp() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(5, 100));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(rawMapping.length - 5, sliced.size());
    Assertions.assertArrayEquals(
        Arrays.stream(rawMapping, 5, rawMapping.length).toArray(),
        ((IndexMapper.ArrayMapping) sliced).mapping);
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
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(rawMapping.length, sliced.size());
    Assertions.assertArrayEquals(
        IntStream.range(0, rawMapping.length)
            .mapToLong(i -> base.map(rawMapping.length - 1 - i))
            .toArray(),
        ((IndexMapper.ArrayMapping) sliced).mapping);
  }

  @Test
  public void mergeReverseAtStart() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(0, 10));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(10, sliced.size());
    Assertions.assertArrayEquals(
        IntStream.range(0, 10).mapToLong(i -> base.map(9 - i)).toArray(),
        ((IndexMapper.ArrayMapping) sliced).mapping);
  }

  @Test
  public void mergeReverseOverflowing() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(10, 100));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(rawMapping.length - 10, sliced.size());
    Assertions.assertArrayEquals(
        IntStream.range(0, rawMapping.length - 10)
            .mapToLong(i -> base.map(rawMapping.length - 1 - i))
            .toArray(),
        ((IndexMapper.ArrayMapping) sliced).mapping);
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
        Arrays.stream(rawMapping, 0, 10).toArray(), ((IndexMapper.ArrayMapping) sliced).mapping);
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
        IntStream.range(0, 5)
            .mapToLong(i -> i == 3 ? IndexMapper.NOT_FOUND_INDEX : base.map(i))
            .toArray(),
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
                LongStream.range(0, 10000).map(idx -> idx % rawMapping.length).toArray()));
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
