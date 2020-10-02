package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

/**
 * Uses Native Image substitution capability to substitute the {@link
 * akka.dispatch.affinity.OnSpinWait.spinWait} function which causes problems when building the
 * Native Image.
 */
@TargetClass(className = "akka.dispatch.affinity.OnSpinWait")
final class ReplacementAkkaSpinWait {

  @Substitute
  public static void spinWait() {
    Thread.onSpinWait();
  }
}
