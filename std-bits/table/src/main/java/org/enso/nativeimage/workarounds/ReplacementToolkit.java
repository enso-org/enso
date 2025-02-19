package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Alias;
import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

@TargetClass(java.awt.Toolkit.class)
final class ReplacementToolkit {

  @Alias private static boolean loaded;

  @Substitute
  static void loadLibraries() {
    loaded = true;
  }

  @Substitute
  private static void initStatic() {}
}
