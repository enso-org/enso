package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Array",
    name = "set_at",
    description = "Puts the given element in the given position in the array.")
public class SetAtNode extends Node {

  Object execute(Array _this, long index, @AcceptsError Object value) {
    try {
      _this.getItems()[(int) index] = value;
      return _this;
    } catch (IndexOutOfBoundsException exception) {
      Builtins builtins = Context.get(this).getBuiltins();
      throw new PanicException(builtins.error().makeInvalidArrayIndexError(_this, index), this);
    }
  }
}
