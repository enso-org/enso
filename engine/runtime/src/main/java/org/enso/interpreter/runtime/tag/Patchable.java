package org.enso.interpreter.runtime.tag;

import org.enso.compiler.core.IR;

public interface Patchable {
  public Object parsePatch(IR.Expression text);

  public final class Tag extends com.oracle.truffle.api.instrumentation.Tag {}
}
