package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.TruffleLanguage;
import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class OtherJvmJavaScriptTest {
  @ClassRule
  public static final ContextUtils ctx = ContextUtils.newBuilder("host").assertGC(false).build();

  private static Channel<OtherJvmPool> CHANNEL;

  @BeforeClass
  public static void initializeChannel() {
    System.setProperty(OtherJvmPool.DUMP_MESSAGE_PROPERTY, "" + Integer.MAX_VALUE);
    CHANNEL = Channel.create(null, OtherJvmPool.class);
    CHANNEL
        .getConfig()
        .onEnterLeave(
            CHANNEL,
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
    assertTrue("Represents a string", returnedResult.isString());

    assertEquals("HelloHelloHello", returnedResult.asString());
    assertEquals("HelloHelloHello", result.value.toString());
  }

  public static String multiString(String txt, int count, ResultCallbacks onResult) {
    try (var jsCtx = Context.newBuilder("js").build()) {
      var fn =
          jsCtx.eval(
              "js",
              """
              (function(txt, count, onResult) {
                  let sb = "";
                  for (let i = 0; i < count; i++) {
                      sb = sb + txt;
                  }
                  onResult.onMessage(sb);
                  return sb;
              })
              """);
      var res = fn.execute(txt, count, onResult).asString();
      return res;
    }
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
