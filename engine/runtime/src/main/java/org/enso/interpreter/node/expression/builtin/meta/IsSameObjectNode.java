package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(
    type = "Meta",
    name = "is_same_object",
    description = "Checks if the two arguments share an underlying reference.",
    autoRegister = false)
public abstract class IsSameObjectNode extends Node {

  public static IsSameObjectNode build() {
    return IsSameObjectNodeGen.create();
  }

  public abstract boolean execute(@AcceptsError Object left, @AcceptsError Object right);

  @Specialization(limit = "3")
  boolean doExecute(
      Object left,
      Object right,
      @CachedLibrary("left") InteropLibrary leftInterop,
      @CachedLibrary("right") InteropLibrary rightInteropp) {
    return (left == right) || leftInterop.isIdentical(left, right, rightInteropp);
  }
}
