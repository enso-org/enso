package org.enso.interpreter.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.Map;
import java.util.UUID;
import org.junit.Test;

public class RuntimeCacheTest {
  @Test
  public void cacheItems() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var obj = 42;

    assertFalse(cache.offer(key, obj));
    assertNull(cache.get(key));

    cache.setWeights(Map.of(key, 1.0));
    assertTrue(cache.offer(key, obj));
    assertEquals(obj, cache.get(key));
  }

  @Test
  public void removeItems() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var obj = new Object();

    cache.setWeights(Map.of(key, 1.0));
    assertTrue(cache.offer(key, obj));
    assertEquals(obj, cache.remove(key));
    assertNull(cache.get(key));
  }

  @Test
  public void cacheTypes() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var obj = "Number";

    assertNull(cache.putType(key, obj));
    assertEquals(obj, cache.putType(key, obj));

    cache.removeType(key);
    assertNull(cache.putType(key, obj));
  }

  @Test
  public void cacheExpressions() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var exprKey = UUID.randomUUID();
    var obj = new Object();

    cache.setWeights(Map.of(key, 1.0));

    assertFalse("Not inserted, as the value isn't in the map yet", cache.offer(exprKey, obj));
    assertNull("No UUID for exprKey", cache.apply(exprKey.toString()));

    assertTrue("key is inserted, as it has associated weight", cache.offer(key, obj));

    assertFalse(
        "obj is already associated with key, will be associated with exprKey",
        cache.offer(exprKey, obj));
    assertEquals("obj inserted", obj, cache.apply(exprKey.toString()));
  }

  @Test
  public void cleanupOfCachedExpressions() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var exprKey = UUID.randomUUID();
    var obj = new Object();

    cache.setWeights(Map.of(key, 1.0));

    assertFalse("Not inserted, as the value isn't in the map yet", cache.offer(exprKey, obj));
    assertNull("No UUID for exprKey", cache.apply(exprKey.toString()));

    assertTrue("key is inserted, as it has associated weight", cache.offer(key, obj));

    assertFalse(
        "obj is already associated with key, will be associated with exprKey",
        cache.offer(exprKey, obj));
    assertEquals("obj inserted", obj, cache.apply(exprKey.toString()));

    var ref = new WeakReference<>(obj);
    obj = null;

    assertGC("Cached object is unlikely to disappear before eviction from the cache", false, ref);

    cache.remove(key);

    assertGC("Cached object can disappear after eviction from the cache", true, ref);
  }

  private static void assertGC(String msg, boolean expectGC, Reference<?> ref) {
    for (var i = 1; i < Integer.MAX_VALUE / 2; i *= 2) {
      if (ref.get() == null) {
        break;
      }
      System.gc();
    }
    var obj = ref.get();
    if (expectGC) {
      assertNull(msg + " ref still alive", obj);
    } else {
      assertNotNull(msg + " ref has been cleaned", obj);
    }
  }
}
