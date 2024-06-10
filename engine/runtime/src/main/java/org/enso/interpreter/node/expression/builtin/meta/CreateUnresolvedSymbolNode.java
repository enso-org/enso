package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.scope.ModuleScope;

@BuiltinMethod(
    type = "Meta",
    name = "create_unresolved_symbol",
    description = "Creates a new unresolved symbol node",
    autoRegister = false)
public abstract class CreateUnresolvedSymbolNode extends Node {
  private @Child ExpectStringNode expectStringNode = ExpectStringNode.build();

  static CreateUnresolvedSymbolNode build() {
    return CreateUnresolvedSymbolNodeGen.create();
  }

  abstract Object execute(Object name, Object symbol);

  @Specialization
  Object doSymbol(Object name, UnresolvedSymbol symbol) {
    return executeWithScope(name, symbol.getScope());
  }

  @Specialization
  Object doConversion(Object name, UnresolvedConversion symbol) {
    return executeWithScope(name, symbol.getScope());
  }

  @Fallback
  ModuleScope doFallback(Object name, Object symbol) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    throw new PanicException(
        builtins.error().makeTypeError("Unresolved_Symbol", symbol, "symbol"), this);
  }

  private Object executeWithScope(Object name, ModuleScope scope) {
    String result = expectStringNode.execute(name);
    if (result.equals(Constants.Names.FROM_MEMBER)) {
      return UnresolvedConversion.build(scope);
    } else {
      return UnresolvedSymbol.build(result, scope);
    }
  }
}
