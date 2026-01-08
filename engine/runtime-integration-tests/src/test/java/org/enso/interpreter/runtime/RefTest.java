package org.enso.interpreter.runtime;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class RefTest {
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.newBuilder().build();

  private static Value refType;

  @BeforeClass
  public static void initCtx() {
    refType =
        ctxRule.evalModule(
            """
            import Standard.Base.Runtime.Ref.Ref
            main = Ref
            """);
  }

  @AfterClass
  public static void closeCtx() throws Exception {
    refType = null;
  }

  private static Value getRef(Value ref) {
    return refType.invokeMember("get", ref);
  }

  private static Value newRef(Object object) {
    return refType.invokeMember("new", refType, object, false);
  }

  @Test
  public void regularReference() throws Exception {
    var obj = new Object();
    var ref = newRef(obj);

    assertFalse("Value returned", ref.isNull());
    assertEquals("Standard.Base.Runtime.Ref.Ref", ref.getMetaObject().getMetaQualifiedName());

    var weakRef = new WeakReference<>(obj);
    obj = null;

    assertEquals("We get the object", weakRef.get(), getRef(ref).asHostObject());

    assertGC("Weak wasn't released", false, weakRef);
    assertFalse("Value was not GCed", getRef(ref).isNull());
    assertEquals("We get the object", weakRef.get(), getRef(ref).asHostObject());

    //    ensoCtx.getReferencesManager().releaseAll();
    assertEquals(
        "releaseAll has no effect on regular reference", weakRef.get(), getRef(ref).asHostObject());
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
