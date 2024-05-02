package org.enso.ydoc.polyfill;

import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

abstract class PolyfillBase implements Polyfill {
  private final String resourceName;

  protected PolyfillBase(String resourceName) {
    this.resourceName = resourceName;
  }

  @Override
  public void initialize(Context ctx) {
    Source jsSource = Source.newBuilder("js", getClass().getResource(resourceName)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }
}
