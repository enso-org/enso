package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.epb.node.CoercePrimitiveNode;
import org.enso.interpreter.node.expression.foreign.CoerceNothing;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Vector",
    name = "unsafe_at",
    description = "Returns an element of Vector at the specified index.")
public class UnsafeAtVectorNode extends Node {

  private @Child CoercePrimitiveNode coercePrimitiveNode = CoercePrimitiveNode.build();
  private @Child CoerceNothing coerceNothingNode = CoerceNothing.build();

  java.lang.Object execute(Vector self, long index) {
    try {
      long length = self.getArraySize();
      long idx = index < 0 ? length + index : index;
      if (idx >= length || idx < 0) {
        throw InvalidArrayIndexException.create(index);
      }
      return coerceNothingNode.execute(coercePrimitiveNode.execute(self.readArrayElement(index)));
    } catch (InvalidArrayIndexException e) {
      Context ctx = Context.get(this);
      throw new PanicException(
          ctx.getBuiltins().error().makeInvalidArrayIndexError(self, index), this);
    } catch (UnsupportedMessageException e) {
      final Context ctx = Context.get(this);
      throw new PanicException(ctx.getBuiltins().error().getPolyglotError().wrap(ctx, e), this);
    }
  }
}
