package org.enso.table.data.column.operation.masks;

import java.util.stream.LongStream;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class IndexMapperConstantTests {
  private IndexMapper makeBase() {
    return new IndexMapper.Constant(123);
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
      Assertions.assertEquals(0, base.map(i));
    }
  }

  @Test
  public void rejectsNegativeSize() {
    Assertions.assertThrowsExactly(
        IllegalArgumentException.class, () -> new IndexMapper.Constant(-1));
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
  public void mergeSlice() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(100, sliced.size());
  }

  @Test
  public void mergeTooLongSlice() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 300));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(123, sliced.size());
  }

  @Test
  public void mergeOverflowSlice() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.SingleSlice(100, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(23, sliced.size());
  }

  @Test
  public void failsOnOutOfRangeSlice() {
    var base = makeBase();

    Assertions.assertThrowsExactly(
        IndexOutOfBoundsException.class, () -> base.merge(new IndexMapper.SingleSlice(243, 100)));
  }

  @Test
  public void mergeReverse() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(0, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(100, sliced.size());
  }

  @Test
  public void mergeTooLongReverse() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(0, 300));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(123, sliced.size());
  }

  @Test
  public void mergeOverflowingReverse() {
    var base = makeBase();

    var sliced = base.merge(new IndexMapper.Reversed(100, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(23, sliced.size());
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

    var sliced = base.merge(new IndexMapper.ArrayMapping(new long[] {0, 1, 2, 3, 4}));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(5, sliced.size());
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
        new long[] {0, 0, 0, IndexMapper.NOT_FOUND_INDEX, 0},
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
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
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

  @Test
  public void mergeReturnsSameInstance() {
    var base = makeBase();
    var toMerge = new IndexMapper.SingleSlice(0, 100);

    var first = base.merge(toMerge);
    var second = base.merge(toMerge);
    Assertions.assertSame(first, second);
  }
}
