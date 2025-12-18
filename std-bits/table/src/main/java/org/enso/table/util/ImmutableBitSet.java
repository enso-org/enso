package org.enso.table.util;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.nio.ByteBuffer;
import java.util.BitSet;

/**
 * A wrapper around BitSet that implements boolean operations conveniently. Unlike BitSet,
 * ImmutableBitSet takes a size parameter, which allows .not to be implemented.
 */
public final class ImmutableBitSet {
  private final BitSet bitSet;
  private final int size;
  private ByteBuffer rawData;

  public ImmutableBitSet(BitSet bitSet, int size) {
    this.bitSet = bitSet;
    this.size = size;
  }

  public int cardinality() {
    return Math.min(size, bitSet.cardinality());
  }

  public boolean get(int i) {
    return i < size && bitSet.get(i);
  }

  /**
   * Modifies {@code other} by applying "and" operation with bits in this immutable set to it.
   *
   * @param other bitset to modify
   */
  public void applyAndTo(BitSet other) {
    other.and(bitSet);
  }

  /**
   * Modifies {@code copyTo} bitset by appending bits of this immutable bitset to it at {@code at}
   * position
   *
   * @param copyTo bitset to modify
   * @param at position to append bits to
   * @param length number of bits to copy
   */
  public void copyTo(BitSet copyTo, int at, int length) {
    BitSets.copy(bitSet, copyTo, at, length);
  }

  public ImmutableBitSet and(ImmutableBitSet other) {
    assert size == other.size;
    BitSet result = (BitSet) bitSet.clone();
    result.and(other.bitSet);
    return new ImmutableBitSet(result, size);
  }

  public ImmutableBitSet or(ImmutableBitSet other) {
    assert size == other.size;
    BitSet result = (BitSet) bitSet.clone();
    result.or(other.bitSet);
    return new ImmutableBitSet(result, size);
  }

  public ImmutableBitSet andNot(ImmutableBitSet other) {
    assert size == other.size;
    BitSet result = (BitSet) bitSet.clone();
    result.andNot(other.bitSet);
    return new ImmutableBitSet(result, size);
  }

  public ImmutableBitSet not() {
    BitSet result = (BitSet) bitSet.clone();
    result.flip(0, size);
    return new ImmutableBitSet(result, size);
  }

  public ImmutableBitSet notAnd(ImmutableBitSet other) {
    assert size == other.size;
    BitSet result = (BitSet) bitSet.clone();
    result.flip(0, size);
    result.and(other.bitSet);
    return new ImmutableBitSet(result, size);
  }

  public ImmutableBitSet notAndNot(ImmutableBitSet other) {
    assert size == other.size;
    BitSet result = (BitSet) bitSet.clone();
    result.andNot(other.bitSet);
    return new ImmutableBitSet(result, size);
  }

  public ImmutableBitSet orNot(ImmutableBitSet other) {
    // Doing an extra operation to avoid doing an extra allocation.
    // a || !b => !(!a && b)
    assert size == other.size;
    BitSet result = (BitSet) bitSet.clone();
    result.flip(0, size);
    result.and(other.bitSet);
    result.flip(0, size);
    return new ImmutableBitSet(result, size);
  }

  public static ImmutableBitSet allFalse(int size) {
    return new ImmutableBitSet(new BitSet(), size);
  }

  private static Reference<BitSet> ALL = new WeakReference<>(null);

  public static ImmutableBitSet allTrue(int size) {
    var shared = ALL.get();
    if (shared == null) {
      shared = new BitSet();
      ALL = new WeakReference<>(shared);
    }
    if (shared.length() < size) {
      // fill with true
      shared.set(shared.length(), size);
    }
    return new ImmutableBitSet(shared, size);
  }

  /**
   * Writable copy of the bitset.
   *
   * @return bitset to further modify
   */
  public BitSet cloneBitSet() {
    return (BitSet) bitSet.clone();
  }

  /**
   * Creates an off-heap memory representation of this bitmap.
   *
   * @return buffer to be read by {@link BitSet#valueOf(java.nio.ByteBuffer)}
   */
  public ByteBuffer rawData() {
    if (rawData == null) {
      var bytes = bitSet.toByteArray();
      var sizeInBytes = (size + 7) / 8;
      rawData = ByteBuffer.allocateDirect(sizeInBytes);
      rawData.put(bytes, 0, Math.min(bytes.length, sizeInBytes));
      rawData.position(0);
      rawData.limit(sizeInBytes);
    }
    return rawData;
  }
}
