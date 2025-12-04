package org.enso.table.util;

import static org.junit.Assert.assertEquals;

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
}
