package org.enso.ydoc;

import org.graalvm.polyglot.Context;

public interface Polyfill {

  void initialize(Context ctx);
}
