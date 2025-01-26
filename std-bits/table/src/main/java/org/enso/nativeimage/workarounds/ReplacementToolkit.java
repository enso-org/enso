package org.enso.nativeimage.workarounds;

import com.oracle.svm.core.annotate.Alias;
import com.oracle.svm.core.annotate.TargetClass;
import com.oracle.svm.core.annotate.Substitute;

@TargetClass(java.awt.Toolkit.class)
final class ReplacementToolkit {

    @Alias private static boolean loaded;

    @Substitute
    static void loadLibraries() { loaded = true; }

    @Substitute
    private static  void initStatic() { }
}
