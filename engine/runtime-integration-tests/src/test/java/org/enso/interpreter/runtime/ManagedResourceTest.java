package org.enso.interpreter.runtime;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import org.enso.common.MethodNames;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.hamcrest.Matchers;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class ManagedResourceTest {
  private static Context ctx;
  private static EnsoContext ensoCtx;
  private static Value newResource;
  private static Value createResource;
  private static Value getResource;

  @BeforeClass
  public static void initCtx() throws Exception {
    ctx = ContextUtils.createDefaultContext();
    ensoCtx = ContextUtils.leakContext(ctx);
    var code =
        """
              import Standard.Base.Runtime.Managed_Resource.Managed_Resource

              make_new obj =
                Managed_Resource.register obj (_->0)

              create_new obj system_resource =
                Managed_Resource.register obj (_->0) system_resource

              get_res ref = ref.with it->
                it
              """;
    var src = Source.newBuilder("enso", code, "gc.enso").build();
    var gcEnso = ctx.eval(src);
    newResource = gcEnso.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "make_new");
    createResource = gcEnso.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "create_new");
    getResource = gcEnso.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "get_res");
  }

  @AfterClass
  public static void closeCtx() throws Exception {
    ctx.close();
    ctx = null;
  }

  @Test
  public void regularReference() throws Exception {
    var obj = new Object();
    var ref = newResource.execute(obj);

    assertFalse("Value returned", ref.isNull());
    assertEquals(
        "Standard.Base.Runtime.Managed_Resource.Managed_Resource",
        ref.getMetaObject().getMetaQualifiedName());

    var weakRef = new WeakReference<>(obj);
    obj = null;

    assertEquals("We get the object", weakRef.get(), getResource.execute(ref).asHostObject());

    assertGC("Weak wasn't released", false, weakRef);
    assertFalse("Value was not GCed", getResource.execute(ref).isNull());
    assertEquals("We get the object", weakRef.get(), getResource.execute(ref).asHostObject());

    ensoCtx.getResourceManager().scheduleFinalizationOfSystemReferences();
    assertEquals(
        "scheduleFinalization has no effect on regular reference",
        weakRef.get(),
        getResource.execute(ref).asHostObject());
  }

  @Test
  public void explicitlyReclaimableReference() throws Exception {
    var obj = new Object();
    var ref = createResource.execute(obj, true);

    assertFalse("Value returned", ref.isNull());
    assertEquals(
        "Standard.Base.Runtime.Managed_Resource.Managed_Resource",
        ref.getMetaObject().getMetaQualifiedName());
    assertEquals("We get the object", obj, getResource.execute(ref).asHostObject());

    ensoCtx.getResourceManager().scheduleFinalizationOfSystemReferences();

    var none = getResource.execute(ref);
    assertTrue("Value was GCed", none.isException());
    assertEquals(
        "It is an error", "Standard.Base.Error.Error", none.getMetaObject().getMetaQualifiedName());
    assertThat(
        "Contains Uninitialized_State as payload",
        none.toString(),
        Matchers.allOf(
            Matchers.containsString("Uninitialized_State"),
            Matchers.containsString("Error"),
            Matchers.containsString("Managed_Resource")));
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
