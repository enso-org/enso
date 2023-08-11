package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.Array;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCopyToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Vector",
    name = "insert_builtin",
    description = "Inserts a set of values into the Vector at the specified index.",
    autoRegister = false)
public abstract class InsertBuiltinVectorNode extends Node {
  static InsertBuiltinVectorNode build() {
    return InsertBuiltinVectorNodeGen.create();
  }

  abstract EnsoObject execute(Object vec, long index, Object values);

  @Specialization
  EnsoObject fromVector(
      Vector vec,
      long index,
      Vector values,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(
        ArrayLikeHelpers.vectorToArray(vec),
        index,
        ArrayLikeHelpers.vectorToArray(values),
        copyNode,
        interop);
  }

  @Specialization
  EnsoObject fromArray(
      Array vec,
      long index,
      Vector values,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(vec, index, ArrayLikeHelpers.vectorToArray(values), copyNode, interop);
  }

  @Specialization(guards = "interop.hasArrayElements(vec)")
  EnsoObject fromArrayLike(
      Object vec,
      long index,
      Vector values,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(vec, index, ArrayLikeHelpers.vectorToArray(values), copyNode, interop);
  }

  @Specialization(guards = "interop.hasArrayElements(values)")
  EnsoObject fromVectorWithArrayLikeObject(
      Vector vec,
      long index,
      Object values,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(ArrayLikeHelpers.vectorToArray(vec), index, values, copyNode, interop);
  }

  @Specialization(guards = "interop.hasArrayElements(values)")
  EnsoObject fromArrayWithArrayLikeObject(
      Array vec,
      long index,
      Object values,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(vec, index, values, copyNode, interop);
  }

  @Specialization(guards = {"interop.hasArrayElements(vec)", "interop.hasArrayElements(values)"})
  EnsoObject fromArrayLikeWithArrayLikeObject(
      Object vec,
      long index,
      Object values,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(vec, index, values, copyNode, interop);
  }

  @Fallback
  EnsoObject fromUnknown(Object vec, long index, Object values) {
    throw unsupportedException(values);
  }

  private PanicException unsupportedException(Object values) {
    var ctx = EnsoContext.get(this);
    var err = ctx.getBuiltins().error().makeTypeError("polyglot array", values, "values");
    throw new PanicException(err, this);
  }

  private EnsoObject insertBuiltin(
      Object current,
      long index,
      Object values,
      ArrayLikeCopyToArrayNode copyNode,
      InteropLibrary interop) {
    try {
      long currentLength = interop.getArraySize(current);
      long valuesLength = interop.getArraySize(values);
      Array result = ArrayLikeHelpers.allocate(currentLength + valuesLength);
      copyNode.execute(current, 0, result, 0, index);
      copyNode.execute(values, 0, result, index, valuesLength);
      copyNode.execute(current, index, result, index + valuesLength, currentLength - index);
      return ArrayLikeHelpers.asVectorFromArray(result);
    } catch (UnsupportedMessageException e) {
      throw unsupportedException(values);
    }
  }
}
