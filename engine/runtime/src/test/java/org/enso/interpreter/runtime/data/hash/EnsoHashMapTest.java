package org.enso.interpreter.runtime.data.hash;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class EnsoHashMapTest {
  @Test
  public void avoidOversaturation() {
    var empty = EnsoHashMap.createEmpty();
    var insertNode = HashMapInsertNode.getUncached();
    var joinNode = HashMapInsertAllNode.getUncached();

    var one = insertNode.execute(null, empty, 1, "Jedna");
    var two = insertNode.execute(null, one, 2, "Dvě");
    var three = insertNode.execute(null, two, 3, "Tři");
    assertEquals("Three elements: " + three, 3, three.getHashSize());

    var againOne = joinNode.executeInsertAll(null, one, one, 10);
    assertEquals("Still one: " + againOne, 1, againOne.getHashSize());
  }
}
