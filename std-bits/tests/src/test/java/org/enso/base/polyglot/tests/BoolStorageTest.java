package org.enso.base.polyglot.tests;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.test.utils.ContextUtils;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class BoolStorageTest {
  @ClassRule
  public static final ContextUtils ctx = ContextUtils.newBuilder("enso").assertGC(false).build();

  @BeforeClass
  public static void importAll() {
    ctx.eval("enso", "from Standard.Base import all");
  }

  static Object makeProxy(ColumnStorage<?> storage, Class<?> interfaceClass) {
    InvocationHandler handler =
        (proxy, method, args) -> {
          var proxyMethod = interfaceClass.getMethod(method.getName(), method.getParameterTypes());
          return proxyMethod.invoke(storage, args);
        };
    return Proxy.newProxyInstance(
        interfaceClass.getClassLoader(), new Class[] {interfaceClass}, handler);
  }

  @Test
  public void makeLocalFromLongStorage() {
    var b = Builder.getForBoolean(3);
    b.append(false).appendNulls(1).append(true);
    var storage = b.seal();

    var proxyStorage = (ColumnBooleanStorage) makeProxy(storage, ColumnBooleanStorage.class);
    var localStorage = Builder.makeLocal(proxyStorage);

    assertNotSame("local storage is a copy of storage", storage, localStorage);
    assertEquals("They have the same size", storage.getSize(), localStorage.getSize());
    assertEquals("They have the same type char", storage.typeChar(), localStorage.typeChar());
    assertEquals("They have the same type size", storage.typeSize(), localStorage.typeSize());
    for (var i = 0L; i < storage.getSize(); i++) {
      var elem = storage.getItemBoxed(i);
      var localElem = localStorage.getItemBoxed(i);
      assertEquals("At " + i, elem, localElem);
    }
  }
}
