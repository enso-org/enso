package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Alias;
import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

@TargetClass(className = "java.awt.Toolkit", onlyWith = OnlyWithDesktop.class)
final class Target_Toolkit {

  @Alias private static boolean loaded;

  @Substitute
  static void loadLibraries() {
    loaded = true;
  }

  @Substitute
  private static void initStatic() {}
}
