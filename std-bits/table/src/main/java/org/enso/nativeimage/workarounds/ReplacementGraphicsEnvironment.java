package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.TargetClass;
import com.oracle.svm.core.annotate.Substitute;

@TargetClass(java.awt.GraphicsEnvironment.class)
final class ReplacementGraphicsEnvironment {
    @Substitute
    public static boolean isHeadless() { return true; }
}
