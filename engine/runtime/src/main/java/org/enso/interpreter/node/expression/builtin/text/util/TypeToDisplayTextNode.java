package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;

public final class TypeToDisplayTextNode extends Node {
  @Child private TypeOfNode typeOfNode;

  private TypeToDisplayTextNode(TypeOfNode typeOfNode) {
    this.typeOfNode = typeOfNode;
  }

  public String execute(Object o) {
    return switch (typeOfNode.execute(o)) {
      case Type t -> t.getName();
      default -> fallbackDisplay(o);
    };
  }

  /**
   * Create a node that can display types as text.
   *
   * @return a new type display node
   */
  @NeverDefault
  public static TypeToDisplayTextNode create() {
    return new TypeToDisplayTextNode(TypeOfNode.build());
  }

  public static TypeToDisplayTextNode getUncached() {
    return new TypeToDisplayTextNode(TypeOfNode.getUncached());
  }

  @CompilerDirectives.TruffleBoundary
  private String fallbackDisplay(Object value) {
    var iop = InteropLibrary.getUncached();
    if (value == null) {
      // TODO [RW] This is a temporary workaround to make it possible to display errors related to
      // https://www.pivotaltracker.com/story/show/181652974
      // Most likely it should be removed once that is implemented.
      return "null";
    } else if (iop.hasArrayElements(value)) {
      return "Array";
    } else if (iop.hasMetaObject(value)) {
      try {
        return iop.asString(iop.getMetaSimpleName(iop.getMetaObject(value)));
      } catch (UnsupportedMessageException e) {
        throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);
      }
    } else {
      return "a polyglot object";
    }
  }
}
