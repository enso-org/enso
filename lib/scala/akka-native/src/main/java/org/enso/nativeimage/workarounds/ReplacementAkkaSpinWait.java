package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

@TargetClass(className = "akka.dispatch.affinity.OnSpinWait")
final class ReplacementAkkaSpinWait {
  @Substitute
  public static void spinWait() {
    Thread.onSpinWait();
  }
}
