package org.enso.interpreter.instrument;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

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
}
