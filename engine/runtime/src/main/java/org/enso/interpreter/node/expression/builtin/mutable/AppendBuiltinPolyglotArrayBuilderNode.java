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
    name = "append_builtin",
    description = "Appends a value at a next available index.")
public abstract class AppendBuiltinPolyglotArrayBuilderNode extends Node {
  static AppendBuiltinPolyglotArrayBuilderNode build() {
    return AppendBuiltinPolyglotArrayBuilderNodeGen.create();
  }

  abstract Object execute(Object arr, Object value);

  @Specialization
  Object fromObject(
      org.enso.interpreter.runtime.data.vector.PolyglotArrayBuilder arr,
      Object value,
      @CachedLibrary(limit = "1") InteropLibrary interop,
      @Cached ArrayLikeLengthNode lengthNode) {
    try {
      arr.append(value, interop);
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
              .makeIndexOutOfBounds(arr.getCurrentIndex(), lengthNode.executeLength(arr));
      return DataflowError.withoutTrace(err, this);
    }
    return EnsoContext.get(this).getBuiltins().nothing();
  }
}
