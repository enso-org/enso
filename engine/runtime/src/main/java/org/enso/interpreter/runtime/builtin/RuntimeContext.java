package org.enso.interpreter.runtime.builtin;

import java.util.function.Supplier;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.util.CachingSupplier;

public final class RuntimeContext {

  private final Supplier<Type> type;
  private final CachingSupplier<AtomConstructor> output;
  private final CachingSupplier<AtomConstructor> input;
  private final CachingSupplier<AtomConstructor> dataflowStackTrace;

  RuntimeContext(Supplier<Type> type) {
    this.type = type;
    this.output = CachingSupplier.from(() -> getType().getConstructors().get("Output"));
    this.input = CachingSupplier.from(() -> getType().getConstructors().get("Input"));
    this.dataflowStackTrace =
        CachingSupplier.from(() -> getType().getConstructors().get("Dataflow_Stack_Trace"));
  }

  public Type getType() {
    return type.get();
  }

  public AtomConstructor getOutput() {
    return output.get();
  }

  public AtomConstructor getInput() {
    return input.get();
  }

  public AtomConstructor getDataflowStackTrace() {
    return dataflowStackTrace.get();
  }
}
