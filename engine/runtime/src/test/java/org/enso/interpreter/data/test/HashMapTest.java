package org.enso.interpreter.data.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder;
import org.junit.Test;

public class HashMapTest {
  private static final InteropLibrary interop = InteropLibrary.getUncached();

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
    var map = EnsoHashMap.empty().insert("a", 42);
    assertEquals(1, map.size());
    assertTrue(map.containsKey("a"));
    assertEquals(42, map.get("a"));
  }

  @Test
  public void testBuilder() {
    var builder = EnsoHashMapBuilder.createWithCapacity(10);
    builder.add("a", 42);
    assertEquals(1, builder.getSize());
    EnsoHashMap map = builder.build();
    assertEquals(1, map.size());
    assertTrue(map.containsKey("a"));
    assertEquals(42, map.get("a"));
  }

  @Test
  public void testMultipleSnapshots() {
    var builder = EnsoHashMapBuilder.createWithCapacity(10);
    builder.add("a", 42);
    EnsoHashMap map1 = builder.build();
    builder.add("b", 23);
    EnsoHashMap map2 = builder.build();
    assertEquals(2, builder.getSnapshots().size());
    assertSame("Snapshots are singletons", map1, builder.getSnapshots().get(0));
    assertSame("Snapshots are singletons", map2, builder.getSnapshots().get(1));
    assertTrue(map1.containsKey("a"));
    assertFalse(map1.containsKey("b"));
    assertTrue(map2.containsKey("a"));
    assertTrue(map2.containsKey("b"));
  }

  @Test
  public void testMultipleElementsInsert() {
    var map = EnsoHashMap.empty().insert("a", 42).insert("b", 23).insert("c", 11);
    assertTrue(map.containsKey("a"));
    assertTrue(map.containsKey("b"));
    assertTrue(map.containsKey("c"));
    assertEquals(42, map.get("a"));
    assertEquals(23, map.get("b"));
    assertEquals(11, map.get("c"));
  }

  @Test
  public void testHashMapInterop() throws UnsupportedMessageException, UnknownKeyException {
    var map = EnsoHashMap.empty().insert("a", 42).insert("b", 23);
    assertTrue(interop.hasHashEntries(map));
    assertEquals(2, interop.getHashSize(map));
    assertTrue(interop.isHashEntryExisting(map, "a"));
    assertTrue(interop.isHashEntryExisting(map, "b"));
    assertTrue(interop.isHashEntryReadable(map, "a"));
    assertTrue(interop.isHashEntryReadable(map, "b"));
    assertEquals(42, interop.readHashValue(map, "a"));
    assertEquals(23, interop.readHashValue(map, "b"));
  }
}
