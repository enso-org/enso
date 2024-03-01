package org.enso.interpreter.node.expression.builtin.mutable;

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
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Arrow_Array_Builder",
    name = "set_builtin",
    description = "Sets a value into the Vector at the specified index.")
public abstract class SetBuiltinArrowArrayBuilderNode extends Node {
  static SetBuiltinArrowArrayBuilderNode build() {
    return SetBuiltinArrowArrayBuilderNodeGen.create();
  }

  abstract Object execute(Object arr, long index, Object value);

  @Specialization
  Object fromObject(
      Object arr,
      long index,
      Object value,
      @CachedLibrary(limit = "1") InteropLibrary interop,
      @Cached ArrayLikeLengthNode lengthNode) {
    try {
      interop.writeArrayElement(arr, index, value);
    } catch (UnsupportedMessageException e) {
      var err =
          EnsoContext.get(interop)
              .getBuiltins()
              .error()
              .makeUnsupportedArgumentsError(new Object[] {value}, "invalid argument");
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
