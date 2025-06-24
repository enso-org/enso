package org.enso.table.data.column.operation.masks;

import java.lang.ref.WeakReference;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.LongStream;
import org.enso.table.util.LeastRecentlyUsedCache;

public abstract sealed class IndexMapper
    permits IndexMapper.Constant,
        IndexMapper.SingleSlice,
        IndexMapper.Reversed,
        IndexMapper.ArrayMapping {
  /**
   * A special index value indicating that an index was not found in the mapping. This is used to
   * represent cases where the index does not map to any valid value.
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

    var key = this.uniqueKey * 1_000_000_000 + other.uniqueKey;
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
    if ((index < 0 || index >= size()) && index != NOT_FOUND_INDEX) {
      throw new IndexOutOfBoundsException("Index out of bounds: " + index);
    }
  }

  /** Creates a single constant index mapper that always returns the first value. */
  public static final class Constant extends IndexMapper {
    static IndexMapper throwCantUse() {
      throw new IllegalArgumentException(
          "Constant mapping should only be used to build a single value storage.");
    }

    private final long length;

    public Constant(long length) {
      this.length = length;
    }

    @Override
    public long map(long index) {
      return 0;
    }

    @Override
    public long size() {
      return length;
    }

    @Override
    protected IndexMapper doMerge(IndexMapper other) {
      return switch (other) {
        case Constant constant -> throwCantUse();
        case SingleSlice singleSlice -> {
          checkIndexBounds(singleSlice.start);
          long newLength = Math.max(Math.min(length - singleSlice.start, singleSlice.length), 0);
          yield new Constant(newLength);
        }
        case Reversed reversed -> {
          checkIndexBounds(reversed.start);
          long newLength = Math.max(Math.min(length - reversed.start, reversed.length), 0);
          yield new Constant(newLength);
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
              newMask[i] = arrayMapping.mapping[i];
            }
          }
          yield hasNegativeOne ? new ArrayMapping(newMask) : new Constant(newMask.length);
        }
      };
    }
  }

  public static final class Reversed extends IndexMapper {
    private final long start;
    private final long length;

    public Reversed(long start, long length) {
      if (start < 0 || length < 0) {
        throw new IllegalArgumentException("Start and length must be non-negative");
      }
      this.start = start;
      this.length = length;
    }

    @Override
    public long map(long index) {
      return start + length - 1 - index;
    }

    @Override
    public long size() {
      return length;
    }

    @Override
    protected IndexMapper doMerge(IndexMapper other) {
      return switch (other) {
        case Constant constant -> Constant.throwCantUse();
        case SingleSlice slice -> {
          if (slice.start > length) {
            yield new Reversed(slice.start, 0);
          }
          long newLength = Math.min(length - slice.start, slice.length);
          long newStart = start + length - newLength;
          yield new Reversed(newStart, newLength);
        }
        case Reversed reversed -> {
          if (reversed.start > length) {
            yield new SingleSlice(reversed.start, 0);
          }
          long newLength = Math.min(length - reversed.start, reversed.length);
          long newStart = start + length - newLength;
          yield new SingleSlice(newStart, newLength);
        }
        case ArrayMapping arrayMapping -> remap(this, arrayMapping.mapping);
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
        case Constant constant -> Constant.throwCantUse();
        case SingleSlice slice -> {
          long newStart = Math.min(start + length, map(slice.start));
          long newLength = Math.max(0, Math.min(length - slice.start, slice.length));
          yield new SingleSlice(newStart, newLength);
        }
        case Reversed reversed -> {
          long newStart = Math.min(start + length, map(reversed.start));
          long newLength = Math.max(0, Math.min(length - reversed.start, reversed.length));
          yield new Reversed(newStart, newLength);
        }
        case ArrayMapping arrayMapping -> remap(this, arrayMapping.mapping);
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
        case Constant constant -> Constant.throwCantUse();
        case SingleSlice slice -> {
          if (slice.start > mapping.length) {
            yield new SingleSlice(slice.start, 0);
          }
          long newLength = Math.min(mapping.length + slice.start, slice.length);
          long[] newMapping =
              Arrays.copyOfRange(mapping, (int) slice.start, (int) (slice.start + newLength));
          yield new ArrayMapping(newMapping);
        }
        case Reversed reversed -> {
          if (reversed.start > mapping.length) {
            yield new SingleSlice(reversed.start, 0);
          }
          long newLength = Math.min(mapping.length + reversed.start, reversed.length);
          long[] newMapping =
              LongStream.range(0, newLength).map(i -> map(reversed.map(i))).toArray();
          yield new ArrayMapping(newMapping);
        }
        case ArrayMapping arrayMapping -> remap(this, arrayMapping.mapping);
      };
    }
  }

  private static IndexMapper.ArrayMapping remap(IndexMapper mapper, long[] rawMapping) {
    long[] newMask = new long[rawMapping.length];
    for (int i = 0; i < rawMapping.length; i++) {
      mapper.checkIndexBounds(rawMapping[i]);
      newMask[i] = rawMapping[i] == NOT_FOUND_INDEX ? NOT_FOUND_INDEX : mapper.map(rawMapping[i]);
    }
    return new ArrayMapping(newMask);
  }
}
