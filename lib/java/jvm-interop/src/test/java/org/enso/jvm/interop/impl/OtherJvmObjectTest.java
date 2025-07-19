package org.enso.jvm.interop.impl;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.math.BigDecimal;
import java.util.function.Consumer;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.hamcrest.CoreMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.core.StringContains;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class OtherJvmObjectTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.newBuilder("js").build();

  private static Channel<OtherJvmPool> CHANNEL;

  @BeforeClass
  public static void initializeChannel() {
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

  @Test
  public void wrapBigDecimal() throws Exception {
    var testClassValue = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    assertOtherJvmObject("Represents clazz from the other JVM", testClassValue);
    var bigReal = newBigDecimal("432.322");
    var otherValue = testClassValue.invokeMember("newBigDecimal", "432.322");
    assertOtherJvmObject("Represents object from the other JVM", otherValue);

    assertFalse("Decimal isn't array", otherValue.hasArrayElements());
    assertEquals(bigReal.toPlainString(), otherValue.invokeMember("toPlainString").asString());

    var twiceReal = bigReal.add(bigReal);
    var twiceValue = otherValue.invokeMember("add", otherValue);
    assertEquals(twiceReal.toBigInteger(), twiceValue.invokeMember("toBigInteger").asBigInteger());
    assertOtherJvmObject("Also other JVM object", twiceValue);

    var minusValue = twiceValue.invokeMember("subtract", otherValue);
    assertEquals(bigReal.toString(), minusValue.invokeMember("toString").asString());
    assertTrue("OtherJvmObject for minus", ctx.unwrapValue(minusValue) instanceof OtherJvmObject);
  }

  public static BigDecimal newBigDecimal(String txt) {
    return new BigDecimal(txt);
  }

  public static Object[] otherJvmArrayWithPrimitives() {
    var bigReal =
        new Object[] {
          "Ahoj", 't', (byte) 1, (short) 2, (int) 3, (long) 4, (float) 5, (double) 6, true
        };
    return bigReal;
  }

  @Test
  public void wrapArray() throws Exception {
    var testClassValue = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var otherValue = testClassValue.invokeMember("otherJvmArrayWithPrimitives");

    assertTrue("Array is array", otherValue.hasArrayElements());
    assertEquals("Few elements", 9, otherValue.getArraySize());
    assertEquals("Ahoj", otherValue.getArrayElement(0).asString());
    assertEquals("t", otherValue.getArrayElement(1).asString());
    assertEquals(1, otherValue.getArrayElement(2).asInt());
    assertEquals(2, otherValue.getArrayElement(3).asInt());
    assertEquals(3, otherValue.getArrayElement(4).asInt());
    assertEquals(4, otherValue.getArrayElement(5).asLong());
    assertEquals(5.0, otherValue.getArrayElement(6).asFloat(), 0.1);
    assertEquals(6.0, otherValue.getArrayElement(7).asDouble(), 0.1);
    assertEquals(true, otherValue.getArrayElement(8).asBoolean());
  }

  @Test
  public void loadClassViaMessage() throws Exception {
    var shortValue = loadOtherJvmClass("java.lang.Short");
    var value = shortValue.invokeMember("valueOf", "32531");
    assertEquals(32531, value.asInt());
  }

  @Test
  public void loadTestClassViaMessage() throws Exception {
    var testClassValue = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var parsedValue = testClassValue.invokeMember("otherJvmValueOf", "32531");
    assertEquals(32531, parsedValue.asInt());
  }

  public static short otherJvmValueOf(String txt) {
    return Short.parseShort(txt);
  }

  @Test
  public void parsingException() throws Exception {
    var shortClass1 = ctx.asValue(java.lang.Short.class).getMember("static");
    try {
      var value1 = shortClass1.invokeMember("valueOf", "not-a-number");
      fail("Unexpected returned value: " + value1);
    } catch (PolyglotException e) {
      MatcherAssert.assertThat(e.getMessage(), CoreMatchers.containsString("not-a-number"));
      assertTrue("This is host exception", e.isHostException());
      assertNotNull("Host exception found", e.asHostException());
    }
    var shortClass2 = loadOtherJvmClass("java.lang.Short");
    try {
      var value2 = shortClass2.invokeMember("valueOf", "not-a-number");
      fail("Unexpected returned value: " + value2);
    } catch (PolyglotException e) {
      MatcherAssert.assertThat(e.getMessage(), CoreMatchers.containsString("not-a-number"));
      assertFalse("Alas this cannot be host exception", e.isHostException());
    }
  }

  @Test
  public void unsupportedOperation() throws Exception {
    var shortClass1 = ctx.asValue(java.lang.Short.class).getMember("static");
    try {
      var value1 = shortClass1.getArrayElement(0);
      fail("Unexpected returned value: " + value1);
    } catch (UnsupportedOperationException e) {
      MatcherAssert.assertThat(
          e.getMessage(), CoreMatchers.containsString("Unsupported operation"));
    }
    var shortClass2 = loadOtherJvmClass("java.lang.Short");
    try {
      var value2 = shortClass2.getArrayElement(0);
      fail("Unexpected returned value: " + value2);
    } catch (UnsupportedOperationException e) {
      MatcherAssert.assertThat(
          e.getMessage(), CoreMatchers.containsString("Unsupported operation"));
    }
  }

  @Test
  public void classNotFoundError() {
    var msg = new OtherJvmMessage.LoadClass("java.lang.unknown.Clazz");
    try {
      var shortRaw = CHANNEL.execute(OtherJvmResult.class, msg).value();
      fail("Should yield an exception: " + shortRaw);
    } catch (ClassNotFoundException ex) {
      assertThat(ex.getMessage(), StringContains.containsString("java.lang.unknown.Clazz"));
    }
  }

  @Test
  public void messageFromAnUnsupportedLibrary() throws Exception {
    var testClassValue = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var bigReal = testClassValue.invokeMember("newBigDecimal", "432.322");
    var other = ctx.unwrapValue(bigReal);
    assertEquals("The right class", OtherJvmObject.class, other.getClass());

    var noType = TypesLibrary.getUncached().hasType(other);
    assertFalse("Other JVM objects don't have type", noType);
  }

  private static final Object IDENTICAL = new Object();

  public static Object otherJvmInstances(int kind) {
    if (kind == 0) {
      return IDENTICAL;
    } else {
      return new Object();
    }
  }

  @Test
  public void isIdenticalCheck() throws Exception {
    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var local1 = localClass.invokeMember("otherJvmInstances", 0);
    var local2 = localClass.invokeMember("otherJvmInstances", 0);
    assertEquals(local1, local2);

    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("otherJvmInstances", 0);
    var other2 = otherClass.invokeMember("otherJvmInstances", 0);
    assertEquals(other1, other2);
  }

  @Test
  public void isNotIdenticalCheck() throws Exception {
    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var local1 = localClass.invokeMember("otherJvmInstances", 1);
    var local2 = localClass.invokeMember("otherJvmInstances", 1);
    assertNotEquals(local1, local2);

    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("otherJvmInstances", 1);
    var other2 = otherClass.invokeMember("otherJvmInstances", 1);
    assertNotEquals(other1, other2);
  }

  public static void callback(Consumer<Object> cb, Object value) {
    cb.accept(value);
  }

  @Test
  public void testCallback() throws Exception {
    class MockProxy implements ProxyExecutable {
      private Value last;

      @Override
      public Object execute(Value... arguments) {
        assertNull("No args yet", last);
        assertEquals("One arg", 1, arguments.length);
        last = arguments[0];

        var myCtx = Context.getCurrent();
        assertEquals("The right context", ctx.context(), myCtx);
        return arguments[0];
      }

      final void assertArgs(String msg, Value exp) {
        assertEquals(msg, exp.asString(), this.last.asString());
        this.last = null;
      }
    }
    var mock = new MockProxy();
    var mockValue = ctx.asValue(mock);

    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());

    localClass.invokeMember("callback", mockValue, "Real");
    mock.assertArgs("Called with Real", ctx.asValue("Real"));

    otherClass.invokeMember("callback", mockValue, "RealOther");
    mock.assertArgs("Called with Real", ctx.asValue("RealOther"));
  }

  private static Value loadOtherJvmClass(String name) throws Exception {
    var msg = new OtherJvmMessage.LoadClass(name);
    var shortRaw = CHANNEL.execute(OtherJvmResult.class, msg).value();
    if (shortRaw instanceof OtherJvmObject other) {
      shortRaw = new OtherJvmObject(CHANNEL, other.id());
    }
    var shortValue = ctx.asValue(shortRaw);
    return shortValue;
  }

  private static void assertOtherJvmObject(String msg, Value value) {
    var unwrap = ctx.unwrapValue(value);
    if (unwrap instanceof OtherJvmObject) {
      return;
    }
    fail(msg + " but got: " + unwrap);
  }
}
