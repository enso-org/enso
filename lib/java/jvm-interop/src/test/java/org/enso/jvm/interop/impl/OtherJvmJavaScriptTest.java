package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.TruffleLanguage;
import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Value;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class OtherJvmJavaScriptTest {
  @ClassRule
  public static final ContextUtils ctx =
      ContextUtils.newBuilder("host", "js")
          .assertGC(false) // but then we cannot try to GC EnsoC`sontext!
          .build();

  private static Channel<OtherJvmPool> CHANNEL;

  @BeforeClass
  public static void initializeChannel() {
    System.setProperty("org.enso.jvm.interop.limit", "" + Integer.MAX_VALUE);
    CHANNEL = Channel.create(null, OtherJvmPool.class);
    CHANNEL
        .getConfig()
        .onEnterLeave(
            FakeLanguage.class,
            null,
            (__) -> {
              ctx.context().enter();
              return null;
            },
            (__, ___) -> {
              ctx.context().leave();
            });
  }

  @Test
  public void wrapTruffleString() throws Exception {
    var testClassValue = loadOtherJvmClass(OtherJvmJavaScriptTest.class.getName());
    assertOtherJvmObject("Represents clazz from the other JVM", testClassValue);

    var result =
        new ResultCallbacks() {
          private Object value;

          @Override
          public void onMessage(Object o) {
            this.value = o;
          }
        };

    var returnedResult = testClassValue.invokeMember("multiString", "Hello", 3, result);
    // assertOtherJvmObject("Represents object from the other JVM", otherValue);

    assertEquals("HelloHelloHello", returnedResult.asString());
    assertEquals("HelloHelloHello", result.value.toString());
  }

  public static String multiString(String txt, int count, ResultCallbacks onResult) {
    StringBuilder sb = new StringBuilder();
    for (var i = 0; i < count; i++) {
      sb.append(txt);
    }
    onResult.onMessage(sb);
    return sb.toString();
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

  private static void assertOtherJvmObject(String msg, Value value) {
    var unwrap = ctx.unwrapValue(value);
    if (unwrap instanceof OtherJvmObject) {
      return;
    }
    fail(msg + " but got: " + unwrap);
  }

  public static interface ResultCallbacks {
    public void onMessage(Object o);
  }

  private abstract static class FakeLanguage extends TruffleLanguage<Object> {}
}
