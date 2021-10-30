package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_unresolved_symbol_name",
    description = "Gets the name of an unresolved symbol")
public class GetUnresolvedSymbolNameNode extends Node {
  Text execute(Object _this, UnresolvedSymbol symbol) {
    return Text.create(symbol.getName());
  }
}
