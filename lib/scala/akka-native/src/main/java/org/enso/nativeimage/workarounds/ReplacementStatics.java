package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;

/**
 * Uses Native Image substitution capability to substitute the {@link
 * scala.runtime.Statics#releaseFence()} function which causes problems when building the Native
 * Image on GraalVM 20.2.0.
 */
@TargetClass(className = "scala.runtime.Statics")
final class ReplacementStatics {

  /**
   * Implements a "release fence" without using an unsupported {@link java.lang.invoke.MethodHandle}
   * like the original one.
   *
   * <p>Instead, uses {@link sun.misc.Unsafe#storeFence()} under the hood.
   */
  @Substitute
  public static void releaseFence() {
    Unsafe.unsafeInstance().storeFence();
  }
}
