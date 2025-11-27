package org.enso.interpreter.runtime.data.atom;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import org.enso.interpreter.node.callable.InteropApplicationNode;
import org.enso.interpreter.runtime.data.EnsoObject;

/**
 * Wrapper for {@link Atom} that is meant to be used by inspector and the debugger. This wrapper
 * changes the behavior of some atom interop messages to be more debugger-friendly. For example, it
 * does not invoke parameter-less instance methods when inspecting the atom. Note that from the
 * engine and language perspective, there is no difference between a field and parameter-less
 * instance method.
 */
@ExportLibrary(value = InteropLibrary.class, delegateTo = "atom")
public final class DebugAtomWrapper extends EnsoObject {
  final Atom atom;

  private DebugAtomWrapper(Atom atom) {
    this.atom = atom;
  }

  public static DebugAtomWrapper wrap(Atom atom) {
    assert atom != null;
    return new DebugAtomWrapper(atom);
  }

  @ExportMessage
  boolean hasMembers() {
    return atom.hasMembers();
  }

  @ExportMessage
  boolean isMemberReadable(String member) {
    return atom.isMemberReadable(member);
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) {
    return atom.getMembers(includeInternal);
  }

  /**
   * Inspired by {@link Atom#readMember(String, StructsLibrary, InteropApplicationNode)}, but does
   * not preapply {@code self} argument if the member is an instance method.
   */
  @ExportMessage
  @ExplodeLoop
  Object readMember(String member, @CachedLibrary(limit = "3") StructsLibrary structsLib)
      throws UnknownIdentifierException {
    // TODO: to_text and to_display_text are exceptions - they should always be
    //  evaluated when the atom is inspected.
    var ctor = atom.getConstructor();
    for (var i = 0; i < ctor.getArity(); i++) {
      var fieldName = ctor.getFields()[i].getName();
      if (fieldName.equals(member)) {
        return structsLib.getField(atom, i);
      }
    }
    var method = atom.findMethod(member);
    if (method != null) {
      return method;
    } else {
      throw UnknownIdentifierException.create(member);
    }
  }

  @ExportMessage
  @Override
  public Object toDisplayString(boolean allowSideEffects) {
    return atom.toDisplayString(allowSideEffects);
  }
}
