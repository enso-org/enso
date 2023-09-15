package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeCopyToArrayNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
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
  EnsoObject flattenAnything(
      Object self,
      @Cached ArrayLikeCopyToArrayNode copyNode,
      @Cached ArrayLikeLengthNode lengthNode,
      @Cached ArrayLikeAtNode atNode) {
    return flatten(self, copyNode, lengthNode, atNode);
  }

  private EnsoObject flatten(
      Object storage,
      ArrayLikeCopyToArrayNode copyNode,
      ArrayLikeLengthNode lengthNode,
      ArrayLikeAtNode atNode) {
    try {
      long length = lengthNode.executeLength(storage);

      long flattened_length = 0;
      for (long i = 0; i < length; i++) {
        var item = atNode.executeAt(storage, i);
        flattened_length += lengthNode.executeLength(item);
      }

      var result = ArrayLikeHelpers.allocate(flattened_length);
      long current_index = 0;
      for (long i = 0; i < length; i++) {
        var item = atNode.executeAt(storage, i);
        var item_length = lengthNode.executeLength(item);
        copyNode.execute(item, 0, result, current_index, item_length);
        current_index += item_length;
      }

      return ArrayLikeHelpers.asVectorFromArray(result);
    } catch (InvalidArrayIndexException e) {
      CompilerDirectives.transferToInterpreter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins.error().makeInvalidArrayIndex(storage, e.getInvalidIndex()), this);
    }
  }
}
