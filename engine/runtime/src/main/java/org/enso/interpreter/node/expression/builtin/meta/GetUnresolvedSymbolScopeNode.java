package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinMethod(
    type = "Meta",
    name = "get_unresolved_symbol_scope",
    description = "Gets the scope of an unresolved symbol",
    autoRegister = false)
public abstract class GetUnresolvedSymbolScopeNode extends Node {
  static GetUnresolvedSymbolScopeNode build() {
    return GetUnresolvedSymbolScopeNodeGen.create();
  }

  abstract ModuleScope execute(Object symbol);

  @Specialization
  ModuleScope doSymbol(UnresolvedSymbol symbol) {
    return symbol.getScope();
  }

  @Specialization
  ModuleScope doConversion(UnresolvedConversion symbol) {
    return symbol.getScope();
  }

  @Fallback
  ModuleScope doFallback(Object symbol) {
    Builtins builtins = Context.get(this).getBuiltins();
    throw new PanicException(
        builtins.error().makeTypeError("Unresolved_Symbol", symbol, "symbol"), this);
  }
}
