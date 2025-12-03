package org.enso.table.util;

import java.util.BitSet;

/**
 * A wrapper around BitSet that implements boolean operations conveniently. Unlike BitSet,
 * ImmutableBitSet takes a size parameter, which allows .not to be implemented.
 */
public final class ImmutableBitSet {
  private final BitSet bitSet;
  private final int size;

  public ImmutableBitSet(BitSet bitSet, int size) {
    this.bitSet = bitSet;
    this.size = size;
  }

  public int cardinality() {
    return bitSet.cardinality();
  }

  public boolean get(int i) {
    return bitSet.get(i);
  }

  public void applyAndTo(BitSet other) {
    other.and(bitSet);
  }

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

  public static ImmutableBitSet allTrue(int size) {
    return new ImmutableBitSet(new BitSet(), size).not();
  }

  public BitSet cloneBitSet() {
    return (BitSet) bitSet.clone();
  }
}
