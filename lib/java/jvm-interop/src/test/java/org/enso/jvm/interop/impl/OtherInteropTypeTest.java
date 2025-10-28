package org.enso.jvm.interop.impl;

import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.TruffleObject;
import java.time.LocalDateTime;
import java.util.Date;
import org.enso.test.utils.ContextUtils;
import org.junit.ClassRule;
import org.junit.Test;

class OtherInteropTypeTest {
  @ClassRule
  public static final ContextUtils ctx =
      ContextUtils.newBuilder("host") // no dynamic languages needed
          .build();

  static int findType(Object obj) {
    var v = ctx.asValue(obj);
    var raw = ctx.unwrapValue(v);
    if (raw instanceof TruffleObject truffle) {
      return OtherInteropType.findType(truffle);
    } else {
      return 0;
    }
  }

  @Test
  public void checkDate() {
    assertTrue("Is date", OtherInteropType.isDate(findType(new Date())));
  }

  @Test
  public void checkDateAndType() {
    var v = findType(LocalDateTime.now());
    assertTrue("Is date", OtherInteropType.isDate(v));
    assertTrue("Is time", OtherInteropType.isTime(v));
  }

  @Test
  public void checkArray() {
    var v = findType(new String[] {"Hi", "there!"});
    assertTrue("Is array", OtherInteropType.hasArrayElements(v));
  }
}
