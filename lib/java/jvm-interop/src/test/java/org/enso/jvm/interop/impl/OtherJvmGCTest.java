package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class OtherJvmGCTest {
  @ClassRule
  public static final ContextUtils ctx =
      ContextUtils.newBuilder("host") // no dynamic languages needed
          .build();

  private static Channel<OtherJvmPool> CHANNEL;

  @BeforeClass
  public static void initializeChannel() {
    System.setProperty("org.enso.jvm.interop.limit", "" + Integer.MAX_VALUE);
    CHANNEL = Channel.create(null, OtherJvmPool.class);
    CHANNEL
        .getConfig()
        .onEnterLeave(
            (__) -> {
              ctx.context().enter();
              return null;
            },
            (__, ___) -> {
              ctx.context().leave();
            });
  }

  public static final class Obj {
    final Holder hold;
    final int id;

    private Obj(Holder hold, int id) {
      this.hold = hold;
      this.id = id;
    }

    public Holder toHolder() {
      return hold;
    }
  }

  public static final class Holder {
    private final Reference<Obj> ref;

    private Holder(Obj[] res, int id) {
      res[0] = new Obj(this, id);
      this.ref = new WeakReference<>(res[0]);
    }

    public final Obj toObj() {
      return ref.get();
    }
  }

  public static Obj holdObj(int v) {
    var arr = new Obj[1];
    var h = new Holder(arr, v);
    assert h.toObj() == arr[0];
    return arr[0];
  }

  public static Reference<Class<OtherJvmGCTest>> getClassReference() {
    return new WeakReference<>(OtherJvmGCTest.class);
  }

  @Test
  public void testGCBehavior() throws Exception {
    var gcClass = loadOtherJvmClass(OtherJvmGCTest.class.getName());
    var objValue = gcClass.invokeMember("holdObj", 34);
    var holdValue = objValue.invokeMember("toHolder");
    assertGC("Cannot GC as we have a reference to objValue", false, holdValue, "toObj");

    var ref = new WeakReference<>(ctx.unwrapValue(objValue));
    objValue = null;
    assertGC("Now it the objValue shall be GCed", true, holdValue, "toObj");
    assertNull("The raw objValue must be gone as well", ref.get());
  }

  @Test
  public void testClassCannotBeGCed() throws Exception {
    var gcClass = loadOtherJvmClass(OtherJvmGCTest.class.getName());
    var refClass = gcClass.invokeMember("getClassReference");
    assertGC("Class cannot GC", false, refClass, "get");
  }

  private static Value loadOtherJvmClass(String name) throws Exception {
    var msg = new OtherJvmMessage.LoadClass(name);
    var raw = CHANNEL.execute(OtherJvmResult.class, msg).value();
    if (raw instanceof OtherJvmObject other) {
      assertTrue(other.assertChannel(CHANNEL));
    }
    var value = ctx.asValue(raw);
    return value;
  }

  private static void assertGC(String msg, boolean expectGC, Value ref, String methodName) {
    Object obj = null;
    for (var i = 1; i < Integer.MAX_VALUE / 2; i *= 2) {
      var value = ref.invokeMember(methodName);
      obj = value.isNull() ? null : ctx.unwrapValue(value);
      if (obj == null) {
        break;
      }
      System.gc();
    }
    if (expectGC) {
      assertNull(msg + " ref still alive", obj);
    } else {
      assertNotNull(msg + " ref has been cleaned", obj);
    }
  }
}
