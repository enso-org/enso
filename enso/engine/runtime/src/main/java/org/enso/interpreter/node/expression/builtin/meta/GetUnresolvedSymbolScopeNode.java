package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinMethod(
    type = "Meta",
    name = "get_unresolved_symbol_scope",
    description = "Gets the scope of an unresolved symbol")
public class GetUnresolvedSymbolScopeNode extends Node {
  ModuleScope execute(Object _this, UnresolvedSymbol symbol) {
    return symbol.getScope();
  }
}
