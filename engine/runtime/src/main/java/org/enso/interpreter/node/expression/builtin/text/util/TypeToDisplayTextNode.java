package org.enso.interpreter.node.expression.builtin.text.util;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.type.TypesGen;

@GenerateUncached
public abstract class TypeToDisplayTextNode extends Node {
  public abstract String execute(Object o);

  /**
   * Create a node that can display types as text.
   *
   * @return a new type display node
   */
  public static TypeToDisplayTextNode build() {
    return TypeToDisplayTextNodeGen.create();
  }

  @Specialization
  @CompilerDirectives.TruffleBoundary
  String doDisplay(
      Object value,
      @CachedLibrary(limit = "5") InteropLibrary objects,
      @CachedLibrary(limit = "5") InteropLibrary displays,
      @CachedLibrary(limit = "5") InteropLibrary strings) {
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
      return value + " (Decimal)";
    } else if (TypesGen.isBoolean(value)) {
      return (TypesGen.asBoolean(value) ? "True" : "False");
    } else if (TypesGen.isText(value)) {
      return "Text";
    } else if (TypesGen.isFunction(value)) {
      return "Function";
    } else if (TypesGen.isAtom(value)) {
      return TypesGen.asAtom(value).getConstructor().getName();
    } else if (TypesGen.isAtomConstructor(value)) {
      return TypesGen.asAtomConstructor(value).getName() + " (Constructor)";
    } else if (TypesGen.isType(value)) {
      return TypesGen.asType(value).getName();
    } else if (TypesGen.isDataflowError(value)) {
      return "Error";
    } else if (TypesGen.isUnresolvedSymbol(value)) {
      return TypesGen.asUnresolvedSymbol(value).getName() + " (Unresolved_Symbol)";
    } else if (TypesGen.isManagedResource(value)) {
      return "Managed_Resource";
    } else if (TypesGen.isArray(value)) {
      return "Array";
    } else if (TypesGen.isRef(value)) {
      return "Ref";
    } else if (objects.hasMetaObject(value)) {
      try {
        return strings.asString(displays.toDisplayString(objects.getMetaObject(value)));
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException("Receiver declares a meta object, but does not return it.");
      }
    } else {
      return "a polyglot object";
    }
  }
}
