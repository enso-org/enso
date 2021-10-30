package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinMethod(
    type = "Meta",
    name = "create_unresolved_symbol",
    description = "Creates a new unresolved symbol node")
public class CreateUnresolvedSymbolNode extends Node {
  private @Child ExpectStringNode expectStringNode = ExpectStringNode.build();

  UnresolvedSymbol execute(Object _this, Object name, ModuleScope scope) {
    return UnresolvedSymbol.build(expectStringNode.execute(name), scope);
  }
}
