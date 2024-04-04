package org.enso.interpreter.node.expression.builtin.runtime;

import java.util.List;
import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.Builtin;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;

@BuiltinType
public class Context extends Builtin {
  @Override
  protected List<Cons> getDeclaredConstructors() {
    return List.of(
        new Cons(INPUT_NAME), new Cons(OUTPUT_NAME), new Cons(DATAFLOW_STACK_TRACE_NAME));
  }

  public static final String INPUT_NAME = "Input";

  public static final String OUTPUT_NAME = "Output";

  public static final String DATAFLOW_STACK_TRACE_NAME = "Dataflow_Stack_Trace";

  public AtomConstructor getInput() {
    return getConstructors()[0];
  }

  public AtomConstructor getOutput() {
    return getConstructors()[1];
  }

  public AtomConstructor getDataflowStackTrace() {
    return getConstructors()[2];
  }
}
