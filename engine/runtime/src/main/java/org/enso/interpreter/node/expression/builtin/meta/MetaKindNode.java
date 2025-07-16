package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Meta",
    name = "get_kind_builtin",
    description = "Finds kind of a value",
    autoRegister = false)
final class MetaKindNode extends Node {
  final Object execute(@AcceptsError Object value) {
    if (value instanceof Atom) {
      return 2L;
    }
    var ctx = EnsoContext.get(this);
    if (ctx.isJavaPolyglotObject(value)) {
      return 3L;
    }
    if (value instanceof UnresolvedSymbol || value instanceof UnresolvedConversion) {
      return 4L;
    }
    if (value instanceof DataflowError) {
      return 5L;
    }
    if (value instanceof Type typ) {
      if (typ.getDefinitionScope().getModule().isPrivate()) {
        return 7L;
      }
      return 6L;
    }
    if (value instanceof UnresolvedConstructor) {
      return 8L;
    }
    if (value instanceof Function) {
      return 9L;
    }
    return 0L;
  }
}
