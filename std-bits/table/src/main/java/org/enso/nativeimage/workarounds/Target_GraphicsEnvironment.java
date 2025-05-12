package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

@TargetClass(className = "java.awt.GraphicsEnvironment", onlyWith = OnlyWithDesktop.class)
final class Target_GraphicsEnvironment {
  @Substitute
  public static boolean isHeadless() {
    return true;
  }
}
