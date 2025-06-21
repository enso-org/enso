package org.enso.table.data.column.operation.masks;

import java.lang.ref.WeakReference;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import org.enso.table.util.LeastRecentlyUsedCache;

public abstract sealed class IndexMapper permits IndexMapper.SingleSlice, IndexMapper.ArrayMapping {
  private static Map<Long, WeakReference<IndexMapper>> _mergeCache;

  private static final AtomicLong atomicCounter = new AtomicLong(0);
  private final long uniqueKey = atomicCounter.incrementAndGet();

  abstract long map(long index);

  abstract long size();

  IndexMapper merge(IndexMapper other) {
    if (_mergeCache == null) {
      _mergeCache = new LeastRecentlyUsedCache<>(1000);
    }

    var key = this.uniqueKey * 1000000000 + other.uniqueKey;
    var cached = _mergeCache.get(key);
    if (cached != null) {
      var cachedMapper = cached.get();
      if (cachedMapper != null) {
        return cachedMapper;
      }
    }

    var merged = doMerge(other);
    _mergeCache.put(key, new WeakReference<>(merged));
    return merged;
  }

  protected abstract IndexMapper doMerge(IndexMapper other);

  // ToDo: Constant, Reveresed, ListMapping

  public static final class SingleSlice extends IndexMapper {
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
    protected IndexMapper doMerge(IndexMapper other) {
      return switch (other) {
        case SingleSlice otherSlice -> {
          long newStart = Math.min(start + length, start + otherSlice.start);
          long newLength = Math.max(0, Math.min(length - otherSlice.start, otherSlice.length));
          yield new SingleSlice(newStart, newLength);
        }
        case ArrayMapping arrayMapping -> {
          long[] rawMask = arrayMapping.mapping;
          long[] newMask = new long[rawMask.length];
          for (int i = 0; i < rawMask.length; i++) {
            if (rawMask[i] < -1 || rawMask[i] >= length) {
              throw new IndexOutOfBoundsException("Index out of bounds: " + rawMask[i]);
            }
            newMask[i] = rawMask[i] == -1 ? -1 : rawMask[i] + start;
          }
          yield new ArrayMapping(newMask);
        }
      };
    }
  }

  public static final class ArrayMapping extends IndexMapper {
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
    protected IndexMapper doMerge(IndexMapper other) {
      return switch (other) {
        case SingleSlice singleSlice -> {
          if (singleSlice.start > mapping.length) {
            yield new SingleSlice(singleSlice.start, 0);
          }
          long newLength = Math.min(mapping.length + singleSlice.start, singleSlice.length);
          long[] newMapping =
              Arrays.copyOfRange(
                  mapping, (int) singleSlice.start, (int) (singleSlice.start + newLength));
          yield new ArrayMapping(newMapping);
        }
        case ArrayMapping arrayMapping -> {
          long[] rawMask = arrayMapping.mapping;
          long[] newMask = new long[rawMask.length];
          for (int i = 0; i < rawMask.length; i++) {
            if (rawMask[i] < -1 || rawMask[i] >= mapping.length) {
              throw new IndexOutOfBoundsException("Index out of bounds: " + rawMask[i]);
            }
            newMask[i] = rawMask[i] == -1 ? -1 : mapping[(int) rawMask[i]];
          }
          yield new ArrayMapping(newMask);
        }
      };
    }
  }
}
