package org.enso.interpreter.node.expression.builtin.text.util;

import org.enso.interpreter.node.expression.builtin.meta.TypeOfNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.type.TypesGen;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;

public final class TypeToDisplayTextNode extends Node {
  @Child
  private TypeOfNode typeOfNode;

  private TypeToDisplayTextNode(TypeOfNode typeOfNode) {
    this.typeOfNode = typeOfNode;
  }

  public String execute(Object o) {
    return switch (o) {
      case Type t -> t.getName();
      default -> switch (typeOfNode.execute(o)) {
        case Type t -> t.getName();
        default -> fallbackDisplay(o);
      };
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
    } else if (TypesGen.isLong(value)) {
      return value + " (Integer)";
    } else if (TypesGen.isEnsoBigInteger(value)) {
      return "Integer";
    } else if (TypesGen.isDouble(value)) {
      return value + " (Float)";
    } else if (TypesGen.isBoolean(value)) {
      return (TypesGen.asBoolean(value) ? "True" : "False");
    } else if (TypesGen.isText(value)) {
      return "Text";
    } else if (TypesGen.isFunction(value)) {
      return "Function";
    } else if (value instanceof Atom atom) {
      return atom.getConstructor().getDisplayName();
    } else if (value instanceof AtomConstructor cons) {
      return cons.getType().getName() + "." + cons.getName() + " (Constructor)";
    } else if (TypesGen.isType(value)) {
      return TypesGen.asType(value).getName();
    } else if (TypesGen.isDataflowError(value)) {
      return "Error";
    } else if (TypesGen.isUnresolvedSymbol(value)) {
      return TypesGen.asUnresolvedSymbol(value).getName() + " (Unresolved_Symbol)";
    } else if (TypesGen.isManagedResource(value)) {
      return "Managed_Resource";
    } else if (iop.hasArrayElements(value)) {
      return "Array";
    } else if (TypesGen.isRef(value)) {
      return "Ref";
    } else if (iop.hasMetaObject(value)) {
      try {
        return iop.asString(iop.toDisplayString(iop.getMetaObject(value)));
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException("Receiver declares a meta object, but does not return it.");
      }
    } else {
      if (TypeOfNode.getUncached().execute(value) instanceof Type type) {
        return type.getQualifiedName().toString();
      } else {
        return "a polyglot object";
      }
    }
  }
}
