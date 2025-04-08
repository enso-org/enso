package org.enso.compiler.test.pass;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import java.io.IOException;
import java.util.HashMap;
import java.util.UUID;
import java.util.function.Function;
import org.enso.common.CachePreferences;
import org.enso.persist.Persistance;
import org.junit.BeforeClass;
import org.junit.Test;

public class PassPersistanceTest {
  @BeforeClass
  public static void initializePersistables() {
    org.enso.compiler.core.ir.Persistables.initialize();
    org.enso.compiler.pass.analyse.Persistables.initialize();
  }

  @Test
  public void cachePreferences() throws Exception {
    var idSelf = UUID.randomUUID();
    var idBind = UUID.randomUUID();
    var pref = new CachePreferences(new HashMap<>());
    pref.set(idSelf, CachePreferences.Kind.SELF_ARGUMENT);
    pref.set(idBind, CachePreferences.Kind.BINDING_EXPRESSION);

    var out = serde(CachePreferences.class, pref, -1);
    assertEquals("Two elements", 2, out.preferences().size());
    assertEquals("They are structurally equal", pref, out);
    assertNotSame("But not ==", pref, out);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
    return serde(clazz, l, expectedSize, null);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize, Function<Object, Object> fn)
      throws IOException {
    var arr = Persistance.write(l, fn);
    if (expectedSize >= 0) {
      assertEquals(expectedSize, arr.length - 12);
    }
    var ref = Persistance.read(arr, null);
    return ref.get(clazz);
  }
}
