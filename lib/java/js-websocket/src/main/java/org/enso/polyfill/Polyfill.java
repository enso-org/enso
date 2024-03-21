package org.enso.polyfill;

import org.graalvm.polyglot.Context;

public interface Polyfill {

    void initialize(Context ctx);
}
