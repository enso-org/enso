package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CopyNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;
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

  abstract Vector execute(Object self, long index, Object values);

  @Specialization
  Vector fromVector(
      Vector self,
      long index,
      Vector values,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(self.toArray(), index, values.toArray(), copyNode, interop);
  }

  @Specialization
  Vector fromArray(
      Array self,
      long index,
      Vector values,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(self, index, values.toArray(), copyNode, interop);
  }

  @Specialization(guards = "interop.hasArrayElements(self)")
  Vector fromArrayLike(
      Object self,
      long index,
      Vector values,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(self, index, values.toArray(), copyNode, interop);
  }

  @Specialization(guards = "interop.hasArrayElements(values)")
  Vector fromVectorWithArrayLikeObject(
      Vector self,
      long index,
      Object values,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(self.toArray(), index, values, copyNode, interop);
  }

  @Specialization(guards = "interop.hasArrayElements(values)")
  Vector fromArrayWithArrayLikeObject(
      Array self,
      long index,
      Object values,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(self, index, values, copyNode, interop);
  }

  @Specialization(guards = {"interop.hasArrayElements(self)", "interop.hasArrayElements(values)"})
  Vector fromArrayLikeWithArrayLikeObject(
      Object self,
      long index,
      Object values,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return insertBuiltin(self, index, values, copyNode, interop);
  }

  @Fallback
  Vector fromUnknown(Object self, long index, Object values) {
    throw unsupportedException(values);
  }

  private PanicException unsupportedException(Object values) {
    var ctx = EnsoContext.get(this);
    var err = ctx.getBuiltins().error().makeTypeError("polyglot array", values, "values");
    throw new PanicException(err, this);
  }

  private Vector insertBuiltin(
      Object current, long index, Object values, CopyNode copyNode, InteropLibrary interop) {
    try {
      long current_length = interop.getArraySize(current);
      long values_length = interop.getArraySize(values);
      Array result = Array.allocate(current_length + values_length);
      copyNode.execute(current, 0, result, 0, index);
      copyNode.execute(values, 0, result, index, values_length);
      copyNode.execute(current, index, result, index + values_length, current_length - index);
      return Vector.fromArray(result);
    } catch (UnsupportedMessageException e) {
      throw unsupportedException(values);
    }
  }
}
