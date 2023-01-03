package org.enso.interpreter.data.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.util.Map;
import java.util.Map.Entry;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNodeGen;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNodeGen;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder;
import org.junit.Before;
import org.junit.Test;

public class HashMapTest {
  private InteropLibrary interop;
  private HashCodeAnyNode hashCodeNode;
  private EqualsAnyNode equalsNode;

  @Before
  public void setUp() {
    interop = InteropLibrary.getUncached();
    hashCodeNode = HashCodeAnyNodeGen.getUncached();
    equalsNode = EqualsAnyNodeGen.getUncached();
  }

  @Test
  public void testEmptyMap() throws UnsupportedMessageException {
    var emptyMap = EnsoHashMap.empty();
    assertEquals(0, emptyMap.size());
    Object emptyVec = emptyMap.toVector();
    assertTrue(interop.hasArrayElements(emptyVec));
    assertEquals(0, interop.getArraySize(emptyVec));
  }

  @Test
  public void testElementInsert() {
    var map = EnsoHashMap.empty().insert("a", 42, hashCodeNode, equalsNode);
    assertEquals(1, map.size());
    assertTrue(map.containsKey("a", hashCodeNode, equalsNode));
    assertEquals(42, map.get("a", hashCodeNode, equalsNode));
  }

  @Test
  public void testBuilder() {
    var builder = EnsoHashMapBuilder.create();
    builder.add("a", 42, hashCodeNode, equalsNode);
    assertEquals(1, builder.getSize());
    EnsoHashMap map = builder.build();
    assertEquals(1, map.size());
    assertTrue(map.containsKey("a", hashCodeNode, equalsNode));
    assertEquals(42, map.get("a", hashCodeNode, equalsNode));
  }

  @Test
  public void testMultipleSnapshots() {
    var builder = EnsoHashMapBuilder.create();
    builder.add("a", 42, hashCodeNode, equalsNode);
    EnsoHashMap map1 = builder.build();
    builder.add("b", 23, hashCodeNode, equalsNode);
    EnsoHashMap map2 = builder.build();
    assertEquals(2, builder.getSnapshots().size());
    assertSame("Snapshots are singletons", map1, builder.getSnapshots().get(0));
    assertSame("Snapshots are singletons", map2, builder.getSnapshots().get(1));
    assertTrue(map1.containsKey("a", hashCodeNode, equalsNode));
    assertFalse(map1.containsKey("b", hashCodeNode, equalsNode));
    assertTrue(map2.containsKey("a", hashCodeNode, equalsNode));
    assertTrue(map2.containsKey("b", hashCodeNode, equalsNode));
  }

  @Test
  public void testMultipleElementsInsert() {
    var map = EnsoHashMap.empty()
        .insert("a", 42, hashCodeNode, equalsNode)
        .insert("b", 23, hashCodeNode, equalsNode)
        .insert("c", 11, hashCodeNode, equalsNode);
    assertTrue(map.containsKey("a", hashCodeNode, equalsNode));
    assertTrue(map.containsKey("b", hashCodeNode, equalsNode));
    assertTrue(map.containsKey("c", hashCodeNode, equalsNode));
    assertEquals(42, map.get("a", hashCodeNode, equalsNode));
    assertEquals(23, map.get("b", hashCodeNode, equalsNode));
    assertEquals(11, map.get("c", hashCodeNode, equalsNode));
  }

  /**
   * Insert values with keys that have the same hash code. This will result in a conflict,
   * which will be resolved by linear probing.
   */
  @Test
  public void testInsertElemsWithKeysWithSameHashCode() {
    // hash("Aa") == hash("BB")
    // hash("AaAa") == hash("BBBB")
    var map = EnsoHashMap.empty()
        .insert("Aa", 1, hashCodeNode, equalsNode)
        .insert("BB", 2, hashCodeNode, equalsNode)
        .insert("AaAa", 3, hashCodeNode, equalsNode)
        .insert("BBBB", 4, hashCodeNode, equalsNode);
    assertEquals(4, map.size());
    assertMapEquals(
        Map.of("Aa", 1, "BB", 2, "AaAa", 3, "BBBB", 4),
        map
    );
  }

  @Test
  public void testHashMapInterop() throws UnsupportedMessageException, UnknownKeyException {
    var map = EnsoHashMap.empty()
        .insert("a", 42, hashCodeNode, equalsNode)
        .insert("b", 23, hashCodeNode, equalsNode);
    assertTrue(interop.hasHashEntries(map));
    assertEquals(2, interop.getHashSize(map));
    assertTrue(interop.isHashEntryExisting(map, "a"));
    assertTrue(interop.isHashEntryExisting(map, "b"));
    assertTrue(interop.isHashEntryReadable(map, "a"));
    assertTrue(interop.isHashEntryReadable(map, "b"));
    assertEquals(42, interop.readHashValue(map, "a"));
    assertEquals(23, interop.readHashValue(map, "b"));
  }

  private void assertMapEquals(Map<Object, Object> expectedMap, EnsoHashMap ensoMap) {
    String errMsg = String.format("Expected map=%s, got map=%s, details: ", expectedMap, interop.toDisplayString(ensoMap));
    assertEquals(
        errMsg + "Sizes are different",
        expectedMap.size(),
        ensoMap.size()
    );
    for (Entry<Object, Object> expectedEntry : expectedMap.entrySet()) {
      assertTrue(
          errMsg + "Enso map does not contain " + expectedEntry.getKey() + " key",
          ensoMap.containsKey(expectedEntry.getKey(), hashCodeNode, equalsNode)
      );
      Object actualValue = ensoMap.get(expectedEntry.getKey(), hashCodeNode, equalsNode);
      assertEquals(
          errMsg + "Expected value=" + expectedEntry.getValue() + ", actual value=" + actualValue,
          expectedEntry.getValue(),
          actualValue
      );
    }
  }
}
