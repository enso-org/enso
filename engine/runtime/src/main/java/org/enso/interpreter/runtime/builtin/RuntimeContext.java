package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.CompilerDirectives;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

public final class RuntimeContext {

  private final Type type;

  RuntimeContext(Type type) {
    this.type = type;
  }

  public Type getType() {
    return type;
  }

  @CompilerDirectives.TruffleBoundary
  public AtomConstructor getOutput() {
    return type.getConstructors().get("Output");
  }

  @CompilerDirectives.TruffleBoundary
  public AtomConstructor getInput() {
    return type.getConstructors().get("Input");
  }

  @CompilerDirectives.TruffleBoundary
  public AtomConstructor getDataflowStackTrace() {
    return type.getConstructors().get("Dataflow_Stack_Trace");
  }
}
