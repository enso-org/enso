package org.enso.interpreter.node.expression.builtin.error;

import org.enso.interpreter.dsl.BuiltinType;
import org.enso.interpreter.node.expression.builtin.UniquelyConstructibleBuiltin;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Array;

import java.util.List;

@BuiltinType
public class InvalidArrayIndexError extends UniquelyConstructibleBuiltin {
  @Override
  protected List<String> getConstructorParamNames() {
    return List.of("array", "index");
  }
}
