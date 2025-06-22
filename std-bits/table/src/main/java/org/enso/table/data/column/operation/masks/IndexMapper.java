package org.enso.table.data.column.operation.masks;

import java.lang.ref.WeakReference;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import org.enso.table.util.LeastRecentlyUsedCache;

public abstract sealed class IndexMapper
    permits IndexMapper.Constant, IndexMapper.SingleSlice, IndexMapper.ArrayMapping {
  /**
   * A special index value indicating that an index was not found in the mapping.
   * This is used to represent cases where the index does not map to any valid value.
   */
  public static final long NOT_FOUND_INDEX = -1;

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

  /**
   * Checks if the given index is within the valid bounds of this mapper. Valid indices are from 0
   * to size() - 1, or -1 for special cases.
   *
   * @param index the index to check
   * @throws IndexOutOfBoundsException if the index is out of bounds
   */
  protected void checkIndexBounds(long index) {
    if (index < -1 || index >= size()) {
      throw new IndexOutOfBoundsException("Index out of bounds: " + index);
    }
  }

  // ToDo: Reveresed, ListMapping

  public static final class Constant extends IndexMapper {
    private final long value;
    private final long length;

    public Constant(long value, long length) {
      this.value = value;
      this.length = length;
    }

    @Override
    public long map(long index) {
      return value;
    }

    @Override
    public long size() {
      return length;
    }

    @Override
    protected IndexMapper doMerge(IndexMapper other) {
      return switch (other) {
        case Constant constant -> {
          checkIndexBounds(constant.value);
          yield new Constant(constant.value == NOT_FOUND_INDEX ? NOT_FOUND_INDEX : value + constant.value, constant.length);
        }
        case SingleSlice singleSlice -> {
          if (singleSlice.start > length) {
            yield new SingleSlice(singleSlice.start, 0);
          }
          long newLength = Math.min(length - singleSlice.start, singleSlice.length);
          yield new SingleSlice(singleSlice.start + value, newLength);
        }
        case ArrayMapping arrayMapping -> {
          boolean hasNegativeOne = false;
          long[] newMask = new long[arrayMapping.mapping.length];
          for (int i = 0; i < arrayMapping.mapping.length; i++) {
            checkIndexBounds(arrayMapping.mapping[i]);
            if (newMask[i] == NOT_FOUND_INDEX) {
              newMask[i] = NOT_FOUND_INDEX;
              hasNegativeOne = true;
            } else {
              newMask[i] = value + arrayMapping.mapping[i];
            }
          }
          yield hasNegativeOne ? new ArrayMapping(newMask) : new Constant(value, newMask.length);
        }
      };
    }
  }

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
        case Constant constant -> {
          checkIndexBounds(constant.value);
          yield new Constant(constant.value == NOT_FOUND_INDEX ? NOT_FOUND_INDEX : start + constant.value, constant.length);
        }
        case SingleSlice otherSlice -> {
          long newStart = Math.min(start + length, start + otherSlice.start);
          long newLength = Math.max(0, Math.min(length - otherSlice.start, otherSlice.length));
          yield new SingleSlice(newStart, newLength);
        }
        case ArrayMapping arrayMapping -> {
          long[] newMask = new long[arrayMapping.mapping.length];
          for (int i = 0; i < arrayMapping.mapping.length; i++) {
            checkIndexBounds(arrayMapping.mapping[i]);
            newMask[i] = arrayMapping.mapping[i] == NOT_FOUND_INDEX ? NOT_FOUND_INDEX : arrayMapping.mapping[i] + start;
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
        case Constant constant -> {
          checkIndexBounds(constant.value);
          yield new Constant(
              constant.value == NOT_FOUND_INDEX ? NOT_FOUND_INDEX : mapping[(int) constant.value], constant.length);
        }
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
          long[] newMask = new long[arrayMapping.mapping.length];
          for (int i = 0; i < arrayMapping.mapping.length; i++) {
            checkIndexBounds(arrayMapping.mapping[i]);
            newMask[i] =
                arrayMapping.mapping[i] == NOT_FOUND_INDEX ? NOT_FOUND_INDEX : mapping[(int) arrayMapping.mapping[i]];
          }
          yield new ArrayMapping(newMask);
        }
      };
    }
  }
}
