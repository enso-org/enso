package org.enso.parsinginjs;

import org.apidesign.bck2brwsr.core.JavaScriptPrototype;

class Patching {

    private Patching() {
    }

    static void init0() throws Throwable {
        try {
            // loads the Statics class into the JVM
            scala.runtime.Statics.avalanche(0);
        } catch (Throwable t) {
        }
        try {
            // replaces its class initialization and releaseFence methods
            Statics.releaseFence();
        } catch (Throwable t) {
        }
        // verifies the releaseFence method can be called now
        scala.runtime.Statics.releaseFence();
    }

    /**
     * Patch for {@link scala.runtime.Statics#releaseFence()} method.
     */
    @JavaScriptPrototype(prototype = "", container = "vm.scala_runtime_Statics(false)")
    static class Statics {

        static {
        }

        public static void releaseFence() {
        }
    }
}
