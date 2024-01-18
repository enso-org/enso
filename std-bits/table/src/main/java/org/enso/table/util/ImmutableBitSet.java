package org.enso.table.util;

import java.util.BitSet;
/**
 * A wrapper around BitSet that implements boolean operations conveniently.
 * Unlike BitSet, ImmutableBitSet takes a size parameter, which allows .not to
 * be implemented.
 */
public class ImmutableBitSet {
    private BitSet bitSet;
    private long size;

    public ImmutableBitSet(BitSet bitSet, long size) {
        this.bitSet = bitSet;
        this.size = size;
    }

    public BitSet and(BitSet other) {
        BitSet result = (BitSet) bitSet.clone();
        result.and(other);
        return result;
    }

    public BitSet or(BitSet other) {
        BitSet result = (BitSet) bitSet.clone();
        result.or(other);
        return result;
    }

    public BitSet not() {
        BitSet result = (BitSet) a.clone();
        result.flip(0, size);
        return result;
    }

    public static BitSet allFalse(long size) {
        return new ImmutableBitSet(new BitSet(), size);
    }

    public static BitSet allTrue(long size) {
        return new ImmutableBitSet(new BitSet(), size).not();
    }
}