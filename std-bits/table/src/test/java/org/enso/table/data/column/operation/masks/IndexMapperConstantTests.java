package org.enso.table.data.column.operation.masks;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.stream.LongStream;

public class IndexMapperConstantTests {
  @Test
  public void mergeConstant() {
    var base = new IndexMapper.Constant(123);

    Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
      base.merge(new IndexMapper.Constant(100));
    });
  }

  @Test
  public void mergeSlice() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(100, sliced.size());
  }

  @Test
  public void mergeTooLongSlice() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 300));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(123, sliced.size());
  }

  @Test
  public void mergeOverflowSlice() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.SingleSlice(100, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(23, sliced.size());
  }

  @Test
  public void mergeReverse() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.Reversed(0, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(100, sliced.size());
  }

  @Test
  public void mergeTooLongReverse() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.Reversed(0, 300));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(123, sliced.size());
  }

  @Test
  public void mergeOverflowingReverse() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.Reversed(100, 100));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(23, sliced.size());
  }

  @Test
  public void mergeArrayMapping() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.ArrayMapping(new long[] {0, 1, 2, 3, 4}));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(5, sliced.size());
  }

  @Test
  public void mergeArrayMappingWithNotFound() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.ArrayMapping(new long[] {0, 1, 2, IndexMapper.NOT_FOUND_INDEX, 4}));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(5, sliced.size());
    Assertions.assertArrayEquals(new long[] {0, 0, 0, IndexMapper.NOT_FOUND_INDEX, 0}, ((IndexMapper.ArrayMapping)sliced).mapping);
  }

  @Test
  public void mergeEmptyArrayMapping() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.ArrayMapping(new long[0]));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(0, sliced.size());
  }

  @Test
  public void mergeLargeArrayMapping() {
    var base = new IndexMapper.Constant(123);

    var sliced = base.merge(new IndexMapper.ArrayMapping(LongStream.range(0, 10000).map(idx -> idx % 123).toArray()));
    Assertions.assertInstanceOf(IndexMapper.Constant.class, sliced);
    Assertions.assertEquals(10000, sliced.size());
  }

  @Test
  public void mergeReturnsSameInstance() {
    var base = new IndexMapper.Constant(123);
    var toMerge = new IndexMapper.SingleSlice(0, 100);

    var first = base.merge(toMerge);
    var second = base.merge(toMerge);
    Assertions.assertSame(first, second);
  }
}
