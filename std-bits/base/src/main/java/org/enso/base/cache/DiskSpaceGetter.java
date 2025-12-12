package org.enso.base.cache;

import java.io.File;
import org.enso.base.CurrentEnsoProject;

public class DiskSpaceGetter extends Mockable<Long> {
  @Override
  public Long computeValue() {
    return getRootPath().getUsableSpace();
  }

  private static File getRootPath() {
    var currentEnsoProject = CurrentEnsoProject.get();
    if (currentEnsoProject == null) {
      // If there is no current Enso project, use the current working directory.
      return new File(System.getProperty("user.dir"));
    }
    return new File(CurrentEnsoProject.get().getRootPath());
  }
}
