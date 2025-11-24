package org.enso.jvm.interop.impl;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.lang.foreign.MemorySegment;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.time.Duration;
import java.util.function.Consumer;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.ByteSequence;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.hamcrest.CoreMatchers;
import org.hamcrest.MatcherAssert;
import org.hamcrest.core.StringContains;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;

public class OtherJvmObjectTest {
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

  public static Object wrap(String txt) {
    return new MockString(txt);
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
  public void parsingWithGoodArguments() throws Exception {
    var longClass1 = ctx.asValue(java.lang.Long.class).getMember("static");
    var longClass2 = loadOtherJvmClass(java.lang.Long.class.getName());

    var valid1 = longClass1.invokeMember("parseLong", "42");
    var valid2 = longClass2.invokeMember("parseLong", "42");
    assertEquals(42L, valid1.asLong());
    assertEquals(42L, valid2.asLong());
  }

  @Test
  public void parsingWithWrongArguments() throws Exception {
    var longClass1 = ctx.asValue(java.lang.Long.class).getMember("static");
    var longClass2 = loadOtherJvmClass(java.lang.Long.class.getName());

    try {
      var valid1 = longClass1.invokeMember("parseLong", 42.0);
      fail("Expecting failure: " + valid1);
    } catch (IllegalArgumentException e) {
      // OK
    }
    try {
      var valid2 = longClass2.invokeMember("parseLong", 42.0);
      fail("Expecting failure: " + valid2);
    } catch (IllegalArgumentException e) {
      // OK
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
  public void classNotFoundError() throws Exception {
    try {
      var raw = loadOtherJvmClass("java.lang.unknown.Clazz");
      fail("Should yield an exception: " + raw);
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

  @Test
  public void nullIsSame() throws Exception {
    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("nullInstance");
    var other2 = otherClass.invokeMember("nullInstance");
    assertEquals(other1, other2);

    var raw1 = ctx.unwrapValue(other1);
    var raw2 = ctx.unwrapValue(other2);
    assertSame("The other null is represented by the same instance", raw1, raw2);

    CHANNEL
        .getConfig()
        .assertMessagesCount(
            "No messages for checks on null",
            0,
            () -> {
              assertFalse(other1.isBoolean());
              assertFalse(other1.isException());
              assertFalse(other1.canExecute());
            });
  }

  public static Object nullInstance() {
    return null;
  }

  private static final class MockObject {}

  private static final Object IDENTICAL = new MockObject();

  public static Object otherJvmInstances(int kind) {
    return switch (kind) {
      case 0 -> IDENTICAL;
      case 1 -> new MockObject();
      case 2 -> Duration.ofSeconds(42);
      case 3 -> new int[20];
      case 4 -> "Hello";
      case 5 -> "Hello".repeat(100000);
      default -> null;
    };
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

  @Test
  public void languageCheck() throws Exception {
    var iop = InteropLibrary.getUncached();

    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var local1 = ctx.unwrapValue(localClass.invokeMember("otherJvmInstances", 1));
    assertTrue("it has language", iop.hasLanguage(local1));
    assertEquals("HostLanguage", iop.getLanguage(local1).getSimpleName());

    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = ctx.unwrapValue(otherClass.invokeMember("otherJvmInstances", 1));
    assertTrue("it has language", iop.hasLanguage(other1));
    assertEquals("FakeLanguage", iop.getLanguage(other1).getSimpleName());
  }

  @Test
  public void isDuration() throws Exception {
    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var local1 = localClass.invokeMember("otherJvmInstances", 2);
    assertTrue("Recognized as duration", local1.isDuration());
    var ld = local1.asDuration();

    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("otherJvmInstances", 2);
    assertTrue("Recognized as duration", other1.isDuration());
    var od = other1.asDuration();

    assertEquals(ld, od);
  }

  @Test
  public void isStringShort() throws Exception {
    checkString(4);
  }

  @Test
  public void isStringLong() throws Exception {
    checkString(5);
  }

  private void checkString(int kind) throws Exception {
    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var local1 = localClass.invokeMember("otherJvmInstances", kind);
    assertTrue("Recognized as string", local1.isString());
    var ld = local1.asString();

    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("otherJvmInstances", kind);
    assertTrue("Recognized as string", other1.isString());
    var od = other1.asString();

    assertEquals(ld, od);
  }

  @Test
  public void checkStringLikeIdentity() throws Exception {
    var hello = "Hello World!";

    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var local1 = localClass.invokeMember("wrap", hello);
    assertTrue("Recognized as string", local1.isString());
    var ld = local1.asString();

    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("wrap", hello);
    assertTrue("Recognized as string", other1.isString());
    var od = other1.asString();

    assertEquals(ld, od);
    assertEquals(hello, ld);
    assertEquals(hello, od);

    var lr = ctx.unwrapValue(local1);
    var or = ctx.unwrapValue(other1);
    assertFalse(lr instanceof String);
    assertFalse(or instanceof String);
  }

  @Test
  public void arrayIndexOutOfBounds() throws Exception {
    var localClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    var local1 = localClass.invokeMember("otherJvmInstances", 3);
    assertTrue("Recognized as duration", local1.hasArrayElements());
    assertEquals(20, local1.getArraySize());
    try {
      var res = local1.getArrayElement(200);
      fail("Expecting a failure: " + res);
    } catch (ArrayIndexOutOfBoundsException ex) {
      assertThat(
          ex.getMessage(), StringContains.containsString("Invalid array index 200 for array"));
    }

    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("otherJvmInstances", 3);
    assertTrue("Recognized as duration", other1.hasArrayElements());
    assertEquals(20, other1.getArraySize());
    try {
      var res = other1.getArrayElement(200);
      fail("Expecting a failure: " + res);
    } catch (ArrayIndexOutOfBoundsException ex) {
      assertThat(
          ex.getMessage(), StringContains.containsString("Invalid array index 200 for array"));
    }
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

  @Test
  public void metaObjectEgClassesAreImmutableInJVM() throws Exception {
    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    var other1 = otherClass.invokeMember("otherJvmInstances", 0);
    var clazz1 = other1.getMetaObject();
    assertTrue("Has parents", clazz1.hasMetaParents());
    var other2 = otherClass.invokeMember("otherJvmInstances", 0);
    var clazz2 = other2.getMetaObject();

    CHANNEL
        .getConfig()
        .assertMessagesCount(
            "No messages neded for comparing classes",
            0,
            () -> {
              assertEquals("Classes are the equal (obviously)", clazz1, clazz2);
              assertTrue("First has parents", clazz1.hasMetaParents());
              assertTrue("Second has parents", clazz2.hasMetaParents());
              var rawClass1 = (OtherJvmObject) ctx.unwrapValue(clazz1);
              var rawClass2 = (OtherJvmObject) ctx.unwrapValue(clazz2);
              assertSame("Represented by the same truffle object", rawClass1, rawClass2);
            });
  }

  public static final class WithABuffer {
    public final ByteBuffer buf;

    WithABuffer(ByteBuffer buf) {
      this.buf = buf;
    }

    public String toText() {
      var arr = new byte[buf.limit()];
      buf.get(arr);
      return new String(arr);
    }
  }

  public static WithABuffer withBuffer(int type, int size) {
    var buf =
        switch (type) {
          case 0 -> ByteBuffer.allocateDirect(size);
          default -> throw new IllegalArgumentException();
        };
    buf.put("Hello".getBytes());
    buf.flip();
    return new WithABuffer(buf);
  }

  @Test
  public void directByteBufferHostInterop() throws Exception {
    var otherClass = ctx.asValue(OtherJvmObjectTest.class).getMember("static");
    checkDirectByteBuffer(otherClass);
  }

  @Test
  public void directByteBufferGuestInterop() throws Exception {
    var otherClass = loadOtherJvmClass(OtherJvmObjectTest.class.getName());
    checkDirectByteBuffer(otherClass);
  }

  private void checkDirectByteBuffer(Value otherClass) throws Exception {
    var withBuffer = otherClass.invokeMember("withBuffer", 0, 10);
    var bufValue = withBuffer.getMember("buf");
    assertTrue("It is a byte buffer", bufValue.hasBufferElements());
    var seq = bufValue.as(ByteSequence.class);
    assertEquals('H', seq.byteAt(0));
    assertEquals('e', seq.byteAt(1));
    assertEquals('l', seq.byteAt(2));
    assertEquals('l', seq.byteAt(3));
    assertEquals('o', seq.byteAt(4));

    var buf = asByteBuffer(bufValue);
    assertEquals('H', buf.get(0));
    assertEquals('e', buf.get(1));
    assertEquals('l', buf.get(2));
    assertEquals('l', buf.get(3));
    assertEquals('o', buf.get(4));
    buf.put(0, "Ahoj!".getBytes());

    assertEquals("Ahoj!", withBuffer.invokeMember("toText").asString());
  }

  /**
   * Converting a buffer-like value to {@link ByteBuffer} is tricky. Simple {@link
   * Value#as(java.lang.Class)} works only for {@code HostObject}. To convert "guest value" we need
   * to do something special. Let's rely on special <em>native pointer</em> support provided by the
   * other JVM for direct {@link ByteBuffer}.
   *
   * @param value the value to convert to {@link ByteBuffer}
   * @return instance of {@link ByteBuffer} to use in this JVM
   */
  private static ByteBuffer asByteBuffer(Value value) throws Exception {
    assertTrue("The value is buffer-like", value.hasBufferElements());
    try {
      return value.as(ByteBuffer.class);
    } catch (ClassCastException ex) {
      assertTrue("Direct buffer should support native address", value.isNativePointer());
      var address = value.asNativePointer();
      var seg = MemorySegment.ofAddress(address).reinterpret(value.getBufferSize());
      return seg.asByteBuffer();
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

  @ExportLibrary(InteropLibrary.class)
  static class MockString implements TruffleObject {
    private final String txt;

    private MockString(String txt) {
      this.txt = txt;
    }

    @ExportMessage
    boolean isString() {
      return true;
    }

    @ExportMessage
    String asString() {
      return txt;
    }
  }

  private abstract static class FakeLanguage extends TruffleLanguage<Object> {}
}
