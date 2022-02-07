package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Array",
    name = "at",
    description = "Gets an array element at the given index.")
public class GetAtNode extends Node {

  Object execute(Array _this, long index) {
    try {
      return _this.getItems()[(int) index];
    } catch (IndexOutOfBoundsException exception) {
      Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
      throw new PanicException(builtins.error().makeInvalidArrayIndexError(_this, index), this);
    }
  }
}
