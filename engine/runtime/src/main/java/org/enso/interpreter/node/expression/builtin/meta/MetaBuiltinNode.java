package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.type.TypesGen;

@BuiltinMethod(
    type = "Meta",
    name = "meta_builtin",
    description = "Find the type and constructs a Meta instances.",
    autoRegister = false)
public class MetaBuiltinNode extends Node {
  Object execute(long index, @AcceptsError Object value, Array factories) {
    if (index < 0) {
      index = findIndex(value);
    }
    if (index < 0) {
      return -index;
    }
    var factory = (AtomConstructor) factories.getItems()[(int) index];
    return factory.newInstance(value);
  }

  private int findIndex(Object value) {
    if (TypesGen.isFunction(value)) {
      return 0;
    }
    if (value instanceof Atom) {
      return 1;
    }
    if (TypesGen.isAtomConstructor(value)) {
      return 2;
    }
    if (TypesGen.isUnresolvedSymbol(value) || TypesGen.isUnresolvedConversion(value)) {
      return 4;
    }
    if (TypesGen.isDataflowError(value)) {
      return -5;
    }
    if (TypesGen.isType(value)) {
      return 6;
    }
    var ctx = EnsoContext.get(this);
    if (ctx.getEnvironment().isHostObject(value)) {
      return 3;
    }
    // primitive value
    return 7;
  }
}
