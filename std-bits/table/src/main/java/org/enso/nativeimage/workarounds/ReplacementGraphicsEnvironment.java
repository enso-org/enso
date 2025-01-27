package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

@TargetClass(java.awt.GraphicsEnvironment.class)
final class ReplacementGraphicsEnvironment {
  @Substitute
  public static boolean isHeadless() {
    return true;
  }
}
