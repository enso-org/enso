package org.enso.jvm.interop;

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
  }

  @Test
  public void wrapArray() {
    var bigReal = new String[] {"Ahoj", "there"};
    var bigValue = ctx.asValue(bigReal);
    var bigUnwrap = ctx.unwrapValue(bigValue);
    assertTrue("The value is represented as truffle object", bigUnwrap instanceof TruffleObject);

    var id = OtherMessage.registerObject((TruffleObject) bigUnwrap);
    var other = new OtherJvmObject(CHANNEL, id);
    var otherValue = ctx.asValue(other);

    assertTrue("Aray is array", otherValue.hasArrayElements());
  }
}
