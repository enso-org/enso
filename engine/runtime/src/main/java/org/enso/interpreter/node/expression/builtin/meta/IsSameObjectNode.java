package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
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

  /**
   * Shortcut specialization for meta objects.
   *
   * <p>Note: Do not remove, as this is a workaround for an unexpected behavior of HostObject
   * interop in GraalVM 22.3.0, where isIdentical(ArrayList.class, arrayListObject.getClass()) would
   * return false.
   *
   * @return True if the qualified names of the meta objects are same.
   */
  @Specialization(guards = {"interop.isMetaObject(metaLeft)", "interop.isMetaObject(metaRight)"})
  boolean isSameMetaObjects(
      Object metaLeft, Object metaRight, @CachedLibrary(limit = "2") InteropLibrary interop) {
    try {
      Object metaLeftName = interop.getMetaQualifiedName(metaLeft);
      Object metaRightName = interop.getMetaQualifiedName(metaRight);
      return isIdenticalObjects(metaLeftName, metaRightName, interop);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Fallback
  boolean isIdenticalObjects(
      Object left, Object right, @CachedLibrary(limit = "2") InteropLibrary interop) {
    return (left == right) || interop.isIdentical(left, right, interop);
  }
}
