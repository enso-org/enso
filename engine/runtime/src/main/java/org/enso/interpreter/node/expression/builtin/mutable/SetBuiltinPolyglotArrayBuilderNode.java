package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Polyglot_Array_Builder",
    name = "set_builtin",
    description = "Set a value at a given index.")
public abstract class SetBuiltinPolyglotArrayBuilderNode extends Node {
  static SetBuiltinPolyglotArrayBuilderNode build() {
    return SetBuiltinPolyglotArrayBuilderNodeGen.create();
  }

  abstract Object execute(Object arr, long index, Object value);

  @Specialization
  Object fromObject(
      org.enso.interpreter.runtime.data.vector.PolyglotArrayBuilder arr,
      long index,
      Object value,
      @CachedLibrary(limit = "1") InteropLibrary interop,
      @Cached ArrayLikeLengthNode lengthNode) {
    try {
      interop.writeArrayElement(arr, index, value);
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      Atom err;
      if (e.getCause() != null && e.getCause() instanceof IllegalStateException ise) {
        err = EnsoContext.get(interop).getBuiltins().error().makeInvalidOperation(ise.getMessage());
      } else {
        err =
            EnsoContext.get(interop)
                .getBuiltins()
                .error()
                .makeUnsupportedArgumentsError(new Object[] {value}, "invalid argument");
      }
      return DataflowError.withoutTrace(err, this);
    } catch (UnsupportedTypeException e) {
      throw EnsoContext.get(interop).raiseAssertionPanic(interop, null, e);
    } catch (InvalidArrayIndexException e) {
      var err =
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeIndexOutOfBounds(index, lengthNode.executeLength(arr));
      return DataflowError.withoutTrace(err, this);
    }
    return EnsoContext.get(this).getBuiltins().nothing();
  }
}
