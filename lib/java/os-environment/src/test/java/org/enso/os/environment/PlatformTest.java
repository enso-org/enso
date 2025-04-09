package org.enso.os.environment;

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
    Assert.assertNotNull(DesktopEnvironment.getDirectories());
  }

  @Test
  public void getTrashBin() {
    Assert.assertNotNull(DesktopEnvironment.getTrashBin());
  }
}
