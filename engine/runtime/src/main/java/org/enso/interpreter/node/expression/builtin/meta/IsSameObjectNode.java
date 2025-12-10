package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;

@BuiltinMethod(
    type = "Meta",
    name = "is_same_object",
    description = "Checks if the two arguments share an underlying reference.",
    autoRegister = false)
@GenerateUncached
public abstract class IsSameObjectNode extends Node {

  public static IsSameObjectNode build() {
    return IsSameObjectNodeGen.create();
  }

  public abstract boolean execute(@AcceptsError Object left, @AcceptsError Object right);

  @Specialization
  boolean isSameDouble(double left, double right) {
    if (Double.isNaN(left) && Double.isNaN(right)) {
      return true;
    } else {
      return left == right;
    }
  }

  @Specialization
  boolean isSameLong(long left, long right) {
    return left == right;
  }

  @Specialization(
      guards = {
        "interop.isString(left)",
        "interop.isString(right)",
      })
  boolean isSameString(
      Object left,
      Object right,
      @Shared("interop") @CachedLibrary(limit = "2") InteropLibrary interop) {
    try {
      return interop.asString(left).equals(interop.asString(right));
    } catch (UnsupportedMessageException ex) {
      return false;
    }
  }

  @Specialization
  boolean isSameType(Type typeLeft, Type typeRight) {
    return typeLeft == typeRight;
  }

  @Specialization(
      guards = {
        "interop.isMetaObject(left)",
        "interop.isMetaObject(right)",
      })
  boolean isSameMeta(
      Object left,
      Object right,
      @Shared("interop") @CachedLibrary(limit = "2") InteropLibrary interop) {
    var ctx = EnsoContext.get(this);
    if (ctx.isHostObject(left) && ctx.isHostObject(right)) {
      var hostLeft = ctx.asHostObject(left);
      var hostRight = ctx.asHostObject(right);
      return hostLeft == hostRight;
    }
    return interop.isIdentical(left, right, interop);
  }

  @Fallback
  boolean isIdenticalObjects(
      Object left,
      Object right,
      @Shared("interop") @CachedLibrary(limit = "2") InteropLibrary interop) {
    if (left == right) {
      return true;
    }
    return interop.isIdentical(left, right, interop);
  }
}
