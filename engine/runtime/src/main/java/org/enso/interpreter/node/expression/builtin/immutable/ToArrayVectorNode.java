package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.*;
import org.enso.interpreter.epb.node.CoercePrimitiveNode;
import org.enso.interpreter.node.expression.foreign.CoerceNothing;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.data.Vector;

@BuiltinMethod(
    type = "Vector",
    name = "to_array",
    description = "Returns an Array representation of this Vector.")
public class ToArrayVectorNode extends Node {

  private @Child CoercePrimitiveNode coercePrimitiveNode = CoercePrimitiveNode.build();
  private @Child CoerceNothing coerceNothingNode = CoerceNothing.build();

  org.enso.interpreter.runtime.data.Array execute(Vector self) {
    try {
      if (self.isArray()) {
        return self.toArrayUnsafe();
      }

      long length = self.getArraySize();
      Object[] target = new Object[(int) length];
      for (int i = 0; i < length; i++) {
        try {
          target[i] =
              coerceNothingNode.execute(coercePrimitiveNode.execute(self.readArrayElement(i)));
        } catch (InvalidArrayIndexException e) {
          Builtins builtins = Context.get(this).getBuiltins();
          throw new PanicException(builtins.error().makeInvalidArrayIndexError(self, i), this);
        }
      }
      return new Array(target);
    } catch (com.oracle.truffle.api.interop.UnsupportedMessageException e) {
      Builtins builtins = Context.get(this).getBuiltins();
      throw new PanicException(builtins.error().makePolyglotError(e), this);
    }
  }
}
