package org.enso.table.util;

import static org.junit.Assert.assertEquals;

import java.util.BitSet;
import org.junit.Test;

public final class ImmutableBitSetTest {
  @Test
  public void allTrueCardinality() {
    var five = ImmutableBitSet.allTrue(5);
    assertEquals(5, five.cardinality());
    var ten = ImmutableBitSet.allTrue(10);
    assertEquals(10, ten.cardinality());

    var million = ImmutableBitSet.allTrue(1_000_000);
    assertEquals(1_000_000, million.cardinality());

    var two = ImmutableBitSet.allTrue(2);
    assertEquals(2, two.cardinality());

    assertEquals(5, five.cardinality());
    assertEquals(10, ten.cardinality());
    assertEquals(1_000_000, million.cardinality());
    assertEquals(2, two.cardinality());
  }

  @Test
  public void ninthBitIsOff() {
    var bs = new BitSet();
    bs.set(5);
    var nine = new ImmutableBitSet(bs, 9);
    var buf = nine.rawData();
    assertEquals(0, buf.position());
    assertEquals(2, buf.capacity());
    assertEquals(2, buf.limit());

    var copy = BitSet.valueOf(buf);
    assertEquals(bs, copy);
  }
}
