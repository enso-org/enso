package org.enso.desktopenvironment;

import org.junit.Assert;
import org.junit.Test;

public class PlatformTest {

  @Test
  public void getDirectories() {
    Assert.assertNotNull(Platform.getDirectories());
  }
}
