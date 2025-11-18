package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
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
          .assertGC(false) // but then we cannot try to GC EnsoContext!
          .build();

  private static Channel<OtherJvmPool> CHANNEL;

  @BeforeClass
  public static void initializeChannel() {
    System.setProperty("org.enso.jvm.interop.limit", "" + Integer.MAX_VALUE);
    CHANNEL = Channel.create(null, OtherJvmPool.class);
    CHANNEL
        .getConfig()
        .onEnterLeave(
            null,
            null,
            (__) -> {
              ctx.context().enter();
              return null;
            },
            (__, ___) -> {
              ctx.context().leave();
            });
  }

  public static record Counter(short value) {
    private static short counter;
    private static Reference<Counter> lastCounted = new WeakReference<>(null);

    public static synchronized Counter readCounter() {
      var last = lastCounted.get();
      if (last == null) {
        last = new Counter(++counter);
        lastCounted = new WeakReference<>(last);
      }
      return last;
    }

    public static void tryAndFailToGC() {
      assertGC("This should not GC", false, lastCounted::get, null);
    }

    public static void tryAndSucceedWithGC() {
      assertGC("Now we should GC", true, lastCounted::get, null);
    }

    public static void emptyCall() {}
  }

  @Test
  public void getCounterGCAndGet() throws Exception {
    var counterClass = loadOtherJvmClass(Counter.class.getName());
    var counter = counterClass.invokeMember("readCounter");
    var counterValue = counter.invokeMember("value").asShort();

    counterClass.invokeMember("tryAndFailToGC");
    var counterSame = counterClass.invokeMember("readCounter");
    assertEquals(counter, counterSame);

    counter = null;
    counterSame = null;
    globalFlush =
        () -> {
          counterClass.invokeMember("emptyCall");
        };

    counterClass.invokeMember("tryAndSucceedWithGC");

    var counterDifferent = counterClass.invokeMember("readCounter");
    assertNotEquals(counter, counterDifferent);

    var counterDifferentValue = counterDifferent.invokeMember("value");
    assertEquals(counterValue + 1, counterDifferentValue.asShort());
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

    public final void flush() {}

    @Override
    public String toString() {
      return "Holder{" + "ref=" + toObj() + '}';
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
    var holdValue = assertHolderHolds(gcClass);
    assertGC("Now it the objValue shall be GCed", true, holdValue, "toObj", "flush");
  }

  private Value assertHolderHolds(Value gcClass) {
    var objValue = gcClass.invokeMember("holdObj", 34);
    var holdValue = objValue.invokeMember("toHolder");
    assertGC("Cannot GC as we have a reference to objValue", false, holdValue, "toObj", "flush");

    var ref = new WeakReference<>(ctx.unwrapValue(objValue));
    objValue = null;
    assertGC("The raw objValue must be gone as well", true, ref::get, null);

    return holdValue;
  }

  @Test
  public void testClassCannotBeGCed() throws Exception {
    var gcClass = loadOtherJvmClass(OtherJvmGCTest.class.getName());
    var refClass = gcClass.invokeMember("getClassReference");
    assertGC("Class cannot GC", false, refClass, "get", "get");
  }

  private static Value loadOtherJvmClass(String name) throws Exception {
    var msg = new OtherJvmMessage.LoadClass(name);
    var raw = CHANNEL.execute(OtherJvmResult.class, msg).value(null);
    if (raw instanceof OtherJvmObject other) {
      assertTrue(other.assertChannel(CHANNEL));
    }
    var value = ctx.asValue(raw);
    return value;
  }

  private static Runnable globalFlush;

  private static void assertGC(
      String msg, boolean expectGC, Value ref, String methodName, String flushName) {
    assertGC(
        msg,
        expectGC,
        () -> {
          var value = ref.invokeMember(methodName);
          return value.isNull() ? null : ctx.unwrapValue(value);
        },
        () -> {
          ref.invokeMember(flushName);
        });
  }

  private static void assertGC(String msg, boolean expectGC, Supplier<?> ref, Runnable flush) {
    List<byte[]> alloc = new ArrayList<>();
    for (var i = 1; i < Integer.MAX_VALUE / 2; i *= 2) {
      if (isNull(ref)) {
        break;
      }
      System.gc();
      if (flush != null) {
        flush.run();
      }
      if (globalFlush != null) {
        globalFlush.run();
      }
      alloc.add(new byte[i]);
    }
    if (expectGC) {
      assertNull(msg + " ref still alive " + alloc, ref.get());
    } else {
      assertNotNull(msg + " ref has been cleaned", ref.get());
    }
  }

  private static boolean isNull(Supplier<?> ref) {
    return ref.get() == null;
  }
}
