package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.Array;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCopyToArrayNode;
import org.enso.interpreter.runtime.data.vector.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Vector",
    name = "flatten",
    description = "Flattens a vector of vectors into a single vector.",
    autoRegister = false)
public abstract class FlattenVectorNode extends Node {
  static FlattenVectorNode build() {
    return FlattenVectorNodeGen.create();
  }

  abstract EnsoObject execute(Object self);

  @Specialization
  EnsoObject fromVector(
      Vector self,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      return flatten(ArrayLikeHelpers.vectorToArray(self), copyNode, interop);
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeTypeError(builtins.vector(), self, "self"), this);
    }
  }

  @Specialization
  EnsoObject fromArray(
      Array self,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      return flatten(self, copyNode, interop);
    } catch (UnsupportedMessageException e) {
      throw unsupportedException(self);
    }
  }

  @Specialization(guards = "interop.hasArrayElements(self)")
  EnsoObject fromArrayLike(
      Object self,
      @Shared("copyNode") @Cached ArrayLikeCopyToArrayNode copyNode,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      return flatten(self, copyNode, interop);
    } catch (UnsupportedMessageException e) {
      throw unsupportedException(self);
    }
  }

  @Fallback
  Vector fromUnknown(Object self) {
    throw unsupportedException(self);
  }

  private PanicException unsupportedException(Object self) {
    CompilerDirectives.transferToInterpreter();
    var ctx = EnsoContext.get(this);
    var err = ctx.getBuiltins().error().makeTypeError("polyglot array", self, "self");
    throw new PanicException(err, this);
  }

  private EnsoObject flatten(Object storage, ArrayLikeCopyToArrayNode copyNode, InteropLibrary interop)
      throws UnsupportedMessageException {
    try {
      long length = interop.getArraySize(storage);

      long flattened_length = 0;
      for (long i = 0; i < length; i++) {
        var item = interop.readArrayElement(storage, i);
        if (!interop.hasArrayElements(item)) {
          CompilerDirectives.transferToInterpreter();
          Builtins builtins = EnsoContext.get(this).getBuiltins();
          throw new PanicException(
              builtins.error().makeTypeError(builtins.vector(), item, "[" + i + "]"), this);
        }

        flattened_length += interop.getArraySize(item);
      }

      Array result = ArrayLikeHelpers.allocate(flattened_length);
      long current_index = 0;
      for (long i = 0; i < length; i++) {
        var item = interop.readArrayElement(storage, i);
        var item_length = interop.getArraySize(item);
        copyNode.execute(item, 0, result, current_index, item_length);
        current_index += item_length;
      }

      return ArrayLikeHelpers.asVectorWithCheckAt(result);
    } catch (InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeInvalidArrayIndex(storage, e.getInvalidIndex()), this);
    }
  }
}
