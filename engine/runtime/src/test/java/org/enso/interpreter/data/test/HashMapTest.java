package org.enso.interpreter.data.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.Callable;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNodeGen;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNodeGen;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.junit.Before;
import org.junit.Test;

public class HashMapTest {
  private InteropLibrary interop;
  private HashCodeAnyNode hashCodeNode;
  private EqualsAnyNode equalsNode;
  private Context ctx;

  @Before
  public void setUp() {
    this.ctx = Context.newBuilder("enso")
        .allowExperimentalOptions(true)
        .allowIO(true)
        .allowAllAccess(true)
        .logHandler(new ByteArrayOutputStream())
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        ).build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    execInContext(ctx, () -> {
      this.interop = InteropLibrary.getUncached();
      this.hashCodeNode = HashCodeAnyNodeGen.getUncached();
      this.equalsNode = EqualsAnyNodeGen.getUncached();
      return null;
    });
  }

  private static Value execInContext(Context ctx, Callable<Object> callable) {
    // Force initialization of the context
    ctx.eval("enso", "42");
    ctx.getPolyglotBindings().putMember("testSymbol", (ProxyExecutable) (Value... args) -> {
      try {
        return callable.call();
      } catch (Exception e) {
        throw new AssertionError(e);
      }
    });
    return ctx.getPolyglotBindings().getMember("testSymbol").execute();
  }

  @Test
  public void testEmptyMap() {
    execInContext(ctx, () -> {
      var emptyMap = EnsoHashMap.empty(hashCodeNode, equalsNode);
      assertEquals(0, emptyMap.size());
      Object emptyVec = emptyMap.toVector();
      assertTrue(interop.hasArrayElements(emptyVec));
      assertEquals(0, interop.getArraySize(emptyVec));
      return null;
    });
  }

  @Test
  public void testElementInsert() {
    execInContext(ctx, () -> {
      var map = EnsoHashMap.empty(hashCodeNode, equalsNode).insert("a", 42);
      assertEquals(1, map.size());
      assertTrue(map.containsKey("a"));
      assertEquals(42, map.get("a"));
      return null;
    });
  }

  @Test
  public void testBuilder() {
    execInContext(ctx, () -> {
      var builder = EnsoHashMapBuilder.create(hashCodeNode, equalsNode);
      builder.add("a", 42);
      assertEquals(1, builder.getSize());
      EnsoHashMap map = builder.build();
      assertEquals(1, map.size());
      assertTrue(map.containsKey("a"));
      assertEquals(42, map.get("a"));
      return null;
    });
  }

  @Test
  public void testMultipleSnapshots() {
    execInContext(ctx, () -> {
      var map1 = EnsoHashMap.empty(hashCodeNode, equalsNode).insert("a", 42);
      var map2 = map1.insert("b", 23);
      assertTrue(map1.containsKey("a"));
      assertFalse(map1.containsKey("b"));
      assertTrue(map2.containsKey("a"));
      assertTrue(map2.containsKey("b"));
      return null;
    });
  }

  @Test
  public void testMultipleElementsInsert() {
    execInContext(ctx, () -> {
      var map = EnsoHashMap.empty(hashCodeNode, equalsNode)
          .insert("a", 42)
          .insert("b", 23)
          .insert("c", 11);
      assertTrue(map.containsKey("a"));
      assertTrue(map.containsKey("b"));
      assertTrue(map.containsKey("c"));
      assertEquals(42, map.get("a"));
      assertEquals(23, map.get("b"));
      assertEquals(11, map.get("c"));
      return null;
    });
  }

  /**
   * Insert values with keys that have the same hash code. This will result in a conflict,
   * which will be resolved by linear probing.
   */
  @Test
  public void testInsertElemsWithKeysWithSameHashCode() {
    execInContext(ctx, () -> {
      // hash("Aa") == hash("BB")
      // hash("AaAa") == hash("BBBB")
      var map = EnsoHashMap.empty(hashCodeNode, equalsNode)
          .insert("Aa", 1)
          .insert("BB", 2)
          .insert("AaAa", 3)
          .insert("BBBB", 4);
      assertEquals(4, map.size());
      assertMapEquals(
          Map.of("Aa", 1, "BB", 2, "AaAa", 3, "BBBB", 4),
          map
      );
      return null;
    });
  }

  @Test
  public void testHashMapInterop() {
    execInContext(ctx, () -> {
      var map = EnsoHashMap.empty(hashCodeNode, equalsNode)
          .insert("a", 42)
          .insert("b", 23);
      assertTrue(interop.hasHashEntries(map));
      assertEquals(2, interop.getHashSize(map));
      assertTrue(interop.isHashEntryExisting(map, "a"));
      assertTrue(interop.isHashEntryExisting(map, "b"));
      assertTrue(interop.isHashEntryReadable(map, "a"));
      assertTrue(interop.isHashEntryReadable(map, "b"));
      assertEquals(42, interop.readHashValue(map, "a"));
      assertEquals(23, interop.readHashValue(map, "b"));
      return null;
    });
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
          ensoMap.containsKey(expectedEntry.getKey())
      );
      Object actualValue = ensoMap.get(expectedEntry.getKey());
      assertEquals(
          errMsg + "Expected value=" + expectedEntry.getValue() + ", actual value=" + actualValue,
          expectedEntry.getValue(),
          actualValue
      );
    }
  }
}
