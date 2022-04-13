package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Constants;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "get_unresolved_symbol_name",
    description = "Gets the name of an unresolved symbol")
public abstract class GetUnresolvedSymbolNameNode extends Node {
  static GetUnresolvedSymbolNameNode build() {
    return GetUnresolvedSymbolNameNodeGen.create();
  }

  public static Text fromText = Text.create(Constants.Names.FROM_MEMBER);

  abstract Text execute(Object _this, Object symbol);

  @Specialization
  Text doSymbol(Object _this, UnresolvedSymbol symbol) {
    return Text.create(symbol.getName());
  }

  @Specialization
  Text doConversion(Object _this, UnresolvedConversion symbol) {
    return fromText;
  }

  @Fallback
  Text doFallback(Object _this, Object symbol) {
    Builtins builtins = Context.get(this).getBuiltins();
    throw new PanicException(
        builtins.error().makeTypeError("Unresolved_Symbol", symbol, "symbol"), this);
  }
}
