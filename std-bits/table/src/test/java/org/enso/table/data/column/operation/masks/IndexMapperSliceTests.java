package org.enso.table.data.column.operation.masks;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class IndexMapperSliceTests {
  @Test
  public void mergeConstant() {
    var base = new IndexMapper.SingleSlice(35, 123);

    Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> {
      base.merge(new IndexMapper.Constant(100));
    });
  }

  @Test
  public void mergeSliceAtStart() {
    var base = new IndexMapper.SingleSlice(35, 123);

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 100));
    Assertions.assertInstanceOf(IndexMapper.SingleSlice.class, sliced);
    Assertions.assertEquals(100, sliced.size());
    Assertions.assertEquals(35, ((IndexMapper.SingleSlice) sliced).start);
  }

  @Test
  public void mergeSliceAtStartOverflowing() {
    var base = new IndexMapper.SingleSlice(35, 123);

    var sliced = base.merge(new IndexMapper.SingleSlice(0, 300));
    Assertions.assertInstanceOf(IndexMapper.SingleSlice.class, sliced);
    Assertions.assertEquals(123, sliced.size());
    Assertions.assertEquals(35, ((IndexMapper.SingleSlice) sliced).start);
  }

  @Test
  public void mergeSliceHalfwayUp() {
    var base = new IndexMapper.SingleSlice(35, 123);

    var sliced = base.merge(new IndexMapper.SingleSlice(50, 100));
    Assertions.assertInstanceOf(IndexMapper.SingleSlice.class, sliced);
    Assertions.assertEquals(73, sliced.size());
    Assertions.assertEquals(85, ((IndexMapper.SingleSlice) sliced).start);
  }

  @Test
  public void mergeReverseWholeSlice() {
    var base = new IndexMapper.SingleSlice(35, 123);

    var sliced = base.merge(new IndexMapper.Reversed(0, 123));
    Assertions.assertInstanceOf(IndexMapper.Reversed.class, sliced);
    Assertions.assertEquals(123, sliced.size());
    Assertions.assertEquals(35, ((IndexMapper.Reversed) sliced).start);
  }

  @Test
  public void mergeReverseAtStart() {
    var base = new IndexMapper.SingleSlice(35, 123);

    var sliced = base.merge(new IndexMapper.Reversed(0, 50));
    Assertions.assertInstanceOf(IndexMapper.Reversed.class, sliced);
    Assertions.assertEquals(50, sliced.size());
    Assertions.assertEquals(35, ((IndexMapper.Reversed) sliced).start);
  }

  @Test
  public void mergeReverseOverflowing() {
    var base = new IndexMapper.SingleSlice(35, 123);

    var sliced = base.merge(new IndexMapper.Reversed(80, 100));
    Assertions.assertInstanceOf(IndexMapper.Reversed.class, sliced);
    Assertions.assertEquals(43, sliced.size());
    Assertions.assertEquals(115, ((IndexMapper.Reversed) sliced).start);
  }

  @Test
  public void mergeArrayMapping() {
    var base = new IndexMapper.SingleSlice(35, 123);
    var mapping = new long[] {0, 1, 2, 3, 4, IndexMapper.NOT_FOUND_INDEX, 6, 7, 8, 9};

    var sliced = base.merge(new IndexMapper.ArrayMapping(mapping));
    Assertions.assertInstanceOf(IndexMapper.ArrayMapping.class, sliced);
    Assertions.assertEquals(10, sliced.size());
    Assertions.assertArrayEquals(new long[] {35, 36, 37, 38, 39, IndexMapper.NOT_FOUND_INDEX, 41, 42, 43, 44}, ((IndexMapper.ArrayMapping) sliced).mapping);
  }
}
