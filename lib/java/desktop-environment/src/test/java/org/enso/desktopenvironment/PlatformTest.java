package org.enso.desktopenvironment;

import org.enso.common.Platform;
import org.junit.Assert;
import org.junit.Test;

public class PlatformTest {

  @Test
  public void getOperatingSystem() {
    Assert.assertNotNull(Platform.getOperatingSystem());
  }

  @Test
  public void getDirectories() {
    Assert.assertNotNull(Directories.getForCurrentPlatform());
  }

  @Test
  public void getTrashBin() {
    Assert.assertNotNull(TrashBin.getForCurrentPlatform());
  }
}
