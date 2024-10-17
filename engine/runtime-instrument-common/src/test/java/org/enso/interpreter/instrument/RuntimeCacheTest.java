package org.enso.interpreter.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.HashSet;
import java.util.UUID;
import org.enso.common.CachePreferences;
import org.junit.Test;

public class RuntimeCacheTest {

  @Test
  public void cacheItems() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var obj = 42;

    assertFalse(cache.offer(key, obj));
    assertNull(cache.get(key));

    cache.setPreferences(of(key, CachePreferences.Kind.BINDING_EXPRESSION));
    assertTrue(cache.offer(key, obj));
    assertEquals(obj, cache.get(key));
  }

  @Test
  public void removeItems() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var obj = new Object();

    cache.setPreferences(of(key, CachePreferences.Kind.BINDING_EXPRESSION));
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
  public void cacheAllExpressions() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var exprKey = UUID.randomUUID();
    var obj = new Object();

    cache.setPreferences(of(key, CachePreferences.Kind.BINDING_EXPRESSION));

    assertFalse("Not inserted, as the value isn't in the map yet", cache.offer(exprKey, obj));
    assertNull("No UUID for exprKey in cache", cache.get(exprKey));
    assertEquals("obj inserted into expressions", obj, cache.getAnyValue(exprKey));
    assertEquals("obj inserted into expressions", obj, cache.apply(exprKey.toString()));

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

    cache.setPreferences(of(key, CachePreferences.Kind.BINDING_EXPRESSION));

    assertFalse("Not inserted, as the value isn't in the map yet", cache.offer(exprKey, obj));
    assertNull("No UUID for exprKey in cache", cache.get(exprKey));
    assertEquals("obj inserted into expressions", obj, cache.getAnyValue(exprKey));

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

  @Test
  public void cleanupOfNotCachedExpressions() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var exprKey = UUID.randomUUID();
    var obj = new Object();

    cache.setPreferences(of(key, CachePreferences.Kind.BINDING_EXPRESSION));

    assertFalse("Not inserted, as the value isn't in the map yet", cache.offer(exprKey, obj));
    assertNull("No UUID for exprKey in cache", cache.get(exprKey));
    assertEquals("obj inserted into expressions", obj, cache.getAnyValue(exprKey));

    var ref = new WeakReference<>(obj);
    obj = null;

    assertGC("Local only values are eligible for GC", true, ref);
  }

  /** */
  @Test
  public void runQueryWithCallback() {
    var cache = new RuntimeCache();
    var key = UUID.randomUUID();
    var key2 = UUID.randomUUID();
    var obj = new Object();

    var queried = new HashSet<UUID>();

    var result =
        cache.runQuery(
            queried::add,
            () -> {
              cache.apply(key.toString());
              cache.apply(key2.toString());
              return obj;
            });
    assertEquals(obj, result);

    assertEquals("Two queries to the cache: " + queried, 2, queried.size());
    assertTrue("Two queries to the cache: " + queried, queried.contains(key));
    assertTrue("Two queries to the cache: " + queried, queried.contains(key2));
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

  private static CachePreferences of(UUID key, CachePreferences.Kind value) {
    var preferences = CachePreferences.empty();
    preferences.set(key, value);
    return preferences;
  }
}
