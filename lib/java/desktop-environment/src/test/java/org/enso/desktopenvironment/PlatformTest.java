package org.enso.desktopenvironment;

import org.junit.Assert;
import org.junit.Test;

public class PlatformTest {

  @Test
  public void getOperatingSystem() {
    Assert.assertNotNull(Platform.getOperatingSystem());
  }

  @Test
  public void getDirectories() {
    Assert.assertNotNull(Platform.getOperatingSystem().getDirectories());
  }

  @Test
  public void getTrashBin() {
    Assert.assertNotNull(Platform.getOperatingSystem().getTrashBin());
  }
}
