package org.enso.interpreter.runtime.tag;

public interface Patchable {
  public Object parsePatch(String text);

  public final class Tag extends com.oracle.truffle.api.instrumentation.Tag {}
}
