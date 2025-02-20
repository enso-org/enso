package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Meta",
    name = "get_kind_builtin",
    description = "Finds kind of a value",
    autoRegister = false)
final class MetaKindNode extends Node {
  final long execute(@AcceptsError Object value) {
    if (value instanceof Atom) {
      return 2;
    }
    var ctx = EnsoContext.get(this);
    if (ctx.isJavaPolyglotObject(value)) {
      return 3;
    }
    if (value instanceof UnresolvedSymbol || value instanceof UnresolvedConversion) {
      return 4;
    }
    if (value instanceof DataflowError) {
      return 5;
    }
    if (value instanceof Type) {
      return 6;
    }
    return 0;
  }
}
