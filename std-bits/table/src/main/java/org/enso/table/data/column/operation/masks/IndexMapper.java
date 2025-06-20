package org.enso.table.data.column.operation.masks;

public sealed interface IndexMapper permits IndexMapper.SingleSlice {
  long map(long index);

  long size();

  IndexMapper merge(IndexMapper other);

  final class SingleSlice() implements IndexMapper {
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
      if (other instanceof SingleSlice otherSlice) {
        long newStart = Math.min(start + length, otherSlice.start + start);
        long newEnd = Math.min(start + length, newStart + otherSlice.length);
        return new SingleSlice(newStart, Math.max(0, newEnd - newStart));
      } else {
        throw new IllegalArgumentException("Cannot merge with non-slice IndexMapper");
      }
    }
  }
}
