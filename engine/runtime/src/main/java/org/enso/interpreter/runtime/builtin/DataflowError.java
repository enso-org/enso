package org.enso.interpreter.runtime.builtin;

import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.error.CatchErrorMethodGen;
import org.enso.interpreter.node.expression.builtin.error.ErrorToTextMethodGen;
import org.enso.interpreter.node.expression.builtin.error.GetStackTraceTextMethodGen;
import org.enso.interpreter.node.expression.builtin.error.ThrowErrorMethodGen;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;

public class DataflowError {
  private final AtomConstructor error;

  public DataflowError(Language language, ModuleScope scope) {
    error = new AtomConstructor("Error", scope).initializeFields();

    scope.registerConstructor(error);
    scope.registerMethod(error, "throw", ThrowErrorMethodGen.makeFunction(language));
    scope.registerMethod(error, "catch_primitive", CatchErrorMethodGen.makeFunction(language));
    scope.registerMethod(
        error, "get_stack_trace_text", GetStackTraceTextMethodGen.makeFunction(language));
    scope.registerMethod(error, "to_text", ErrorToTextMethodGen.makeFunction(language));
  }

  public AtomConstructor constructor() {
    return error;
  }
}
