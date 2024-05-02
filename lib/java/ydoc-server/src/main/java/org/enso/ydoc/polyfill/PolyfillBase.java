package org.enso.ydoc.polyfill;

import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

public abstract class PolyfillBase implements Polyfill {
  private final String resourceName;

  protected PolyfillBase(String resourceName) {
    this.resourceName = resourceName;
  }

  @Override
  public void initialize(Context ctx) {
    Source abortControllerJs =
        Source.newBuilder("js", getClass().getResource(resourceName)).buildLiteral();

    ctx.eval(abortControllerJs).execute(this);
  }
}
