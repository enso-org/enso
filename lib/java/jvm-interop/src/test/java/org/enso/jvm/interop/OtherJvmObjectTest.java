package org.enso.jvm.interop;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.TruffleObject;
import java.math.BigDecimal;
import org.enso.jvm.channel.Channel;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

public class OtherJvmObjectTest {
  @ClassRule public static final ContextUtils ctx = ContextUtils.newBuilder("js").build();
  private static final Channel CHANNEL = Channel.create(null, Persistables.class);

  @Test
  public void wrapBigDecimal() {
    var bigReal = new BigDecimal("432.322");
    var bigValue = ctx.asValue(bigReal);
    var bigUnwrap = ctx.unwrapValue(bigValue);
    assertTrue("The value is represented as truffle object", bigUnwrap instanceof TruffleObject);

    var id = OtherMessage.registerObject((TruffleObject) bigUnwrap);
    var other = new OtherJvmObject(CHANNEL, id);
    var otherValue = ctx.asValue(other);

    assertFalse("Decimal isn't array", otherValue.hasArrayElements());
    assertEquals(bigReal.toPlainString(), otherValue.invokeMember("toPlainString").asString());

    var twiceReal = bigReal.add(bigReal);
    var twiceValue = otherValue.invokeMember("add", otherValue);
    assertEquals(twiceReal.toBigInteger(), twiceValue.invokeMember("toBigInteger").asBigInteger());
    assertTrue("It is OtherJvmObject", ctx.unwrapValue(twiceValue) instanceof OtherJvmObject);

    var minusValue = twiceValue.invokeMember("subtract", otherValue);
    assertEquals(bigReal.toString(), minusValue.invokeMember("toString").asString());
    assertTrue("OtherJvmObject for minus", ctx.unwrapValue(minusValue) instanceof OtherJvmObject);
  }

  @Test
  public void wrapArray() {
    var bigReal =
        new Object[] {
          "Ahoj", 't', (byte) 1, (short) 2, (int) 3, (long) 4, (float) 5, (double) 6, true
        };
    var bigValue = ctx.asValue(bigReal);
    var bigUnwrap = ctx.unwrapValue(bigValue);
    assertTrue("The value is represented as truffle object", bigUnwrap instanceof TruffleObject);

    var id = OtherMessage.registerObject((TruffleObject) bigUnwrap);
    var other = new OtherJvmObject(CHANNEL, id);
    var otherValue = ctx.asValue(other);

    assertTrue("Aray is array", otherValue.hasArrayElements());
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
  public void loadAClassMessage() {
    var msg = new OtherMessage.LoadClass("java.lang.Short");
    var shortRaw = CHANNEL.execute(OtherResult.class, msg).value();
    if (shortRaw instanceof OtherJvmObject other) {
      shortRaw = new OtherJvmObject(CHANNEL, other.id());
    }
    var shortValue = ctx.asValue(shortRaw);

    var value = shortValue.invokeMember("valueOf", "32531");
    assertEquals(32531, value.asInt());
  }
}
