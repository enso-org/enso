package org.enso.table.data.column.operation.masks;

import java.util.Arrays;

public sealed interface IndexMapper permits IndexMapper.SingleSlice, IndexMapper.ArrayMapping {
  long map(long index);

  long size();

  IndexMapper merge(IndexMapper other);

  final class SingleSlice implements IndexMapper {
    private final long start;
    private final long length;

    public SingleSlice(long start, long length) {
      if (start < 0 || length < 0) {
        throw new IllegalArgumentException("Start and length must be non-negative");
      }
      this.start = start;
      this.length = length;
    }

    @Override
    public long map(long index) {
      return start + index;
    }

    @Override
    public long size() {
      return length;
    }

    @Override
    public IndexMapper merge(IndexMapper other) {
      return switch (other) {
        case SingleSlice otherSlice -> {
          long newStart = Math.min(start, otherSlice.start);
          long newEnd = Math.max(start + length, otherSlice.start + otherSlice.length);
          yield new SingleSlice(newStart, newEnd - newStart);
        }
        case ArrayMapping arrayMapping -> {
          long[] rawMask = arrayMapping.mapping;
          long[] newMask = new long[rawMask.length];
          for (int i = 0; i < rawMask.length; i++) {
            if (rawMask[i] < start || rawMask[i] >= start + length) {
              throw new IndexOutOfBoundsException("Index out of bounds: " + rawMask[i]);
            }
            newMask[i] = rawMask[i] - start;
          }
          yield new ArrayMapping(newMask);
        }
      };
    }
  }

  final class ArrayMapping implements IndexMapper {
    long[] mapping;

    public ArrayMapping(long[] mapping) {
      if (mapping == null) {
        throw new IllegalArgumentException("Mapping array must not be null.");
      }
      this.mapping = mapping;
    }

    @Override
    public long map(long index) {
      if (index < 0 || index >= mapping.length) {
        throw new IndexOutOfBoundsException("Index out of bounds: " + index);
      }
      return mapping[(int) index];
    }

    @Override
    public long size() {
      return mapping.length;
    }

    @Override
    public IndexMapper merge(IndexMapper other) {
      return switch (other) {
        case SingleSlice singleSlice -> {
          if (singleSlice.start > mapping.length) {
            yield new SingleSlice(singleSlice.start, 0);
          }
          long newLength = Math.min(mapping.length + singleSlice.start, singleSlice.length);
          long[] newMapping = Arrays.copyOfRange(mapping, (int) singleSlice.start, (int) newLength);
          yield new ArrayMapping(newMapping);
        }
        case ArrayMapping arrayMapping -> {
          long[] rawMask = arrayMapping.mapping;
          long[] newMask = new long[rawMask.length];
          for (int i = 0; i < rawMask.length; i++) {
            if (rawMask[i] < 0 || rawMask[i] >= mapping.length) {
              throw new IndexOutOfBoundsException("Index out of bounds: " + rawMask[i]);
            }
            newMask[i] = mapping[(int) rawMask[i]];
          }
          yield new ArrayMapping(newMask);
        }
      };
    }
  }
}
