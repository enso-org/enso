package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
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
    name = "remove_builtin",
    description = "Removes a value for the vector at the specified index.",
    autoRegister = false)
public abstract class RemoveAtVectorNode extends Node {
  static RemoveAtVectorNode build() {
    return RemoveAtVectorNodeGen.create();
  }

  abstract Vector execute(Object self, long index);

  @Specialization
  Vector fromVector(
      Vector self,
      long index,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return removeAtIndex(self.toArray(), index, copyNode, interop);
  }

  @Specialization
  Vector fromArray(
      Array self,
      long index,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return removeAtIndex(self, index, copyNode, interop);
  }

  @Specialization(guards = "interop.hasArrayElements(self)")
  Vector fromArrayLike(
      Object self,
      long index,
      @Cached CopyNode copyNode,
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    return removeAtIndex(self, index, copyNode, interop);
  }

  @Fallback
  Vector fromUnknown(Object self, long index) {
    throw unsupportedException(self);
  }

  private PanicException unsupportedException(Object self) {
    var ctx = EnsoContext.get(this);
    var err = ctx.getBuiltins().error().makeTypeError("polyglot array", self, "self");
    throw new PanicException(err, this);
  }

  private Vector removeAtIndex(
      Object storage, long index, CopyNode copyArrayNode, InteropLibrary interop) {
    try {
      long length = interop.getArraySize(storage);
      long actualIndex = index < 0 ? index + length : index;
      Array array = Array.allocate(length - 1);
      copyArrayNode.execute(storage, 0, array, 0, actualIndex);
      copyArrayNode.execute(storage, actualIndex + 1, array, actualIndex, length - actualIndex - 1);
      return Vector.fromArray(array);
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(e);
    }
  }
}
