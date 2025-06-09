package org.enso.jvm.interop;

import org.junit.Assert;
import org.junit.Test;

public class OtherJvmObjectTest {
  @Test
  public void connect() {
    var to = new OtherJvmObject();
    Assert.fail("OKeyish" + to);
  }
}
