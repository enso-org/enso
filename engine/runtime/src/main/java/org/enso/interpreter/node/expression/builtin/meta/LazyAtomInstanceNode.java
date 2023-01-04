package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.mutable.CoerceArrayNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Meta",
    name = "lazy_atom",
    description = "Creates a new atom with given constructor and fields.",
    autoRegister = false)
public abstract class LazyAtomInstanceNode extends Node {

  static LazyAtomInstanceNode build() {
    return LazyAtomInstanceNodeGen.create();
  }

  abstract Vector execute(Object factory);

  @Specialization(guards = "iop.isExecutable(factory)")
  Vector doExecute(Object factory, @CachedLibrary(limit="3") InteropLibrary iop, @Cached CoerceArrayNode coerce) {
    var ctx = EnsoContext.get(this);
    var lazy = new HoleInAtom(null, -1, null);
    try {
      var r = iop.execute(factory, lazy);
      if (r instanceof Atom a) {
        for (int i = 0; i < a.getFields().length; i++) {
          if (a.getFields()[i] == lazy) {
            return Vector.fromArray(new Array(a, new HoleInAtom(a, i, lazy)));
          }
        }
      }
      throw new PanicException(ctx.getBuiltins().error().getInvalidArrayIndex(), this);
    } catch (UnsupportedTypeException ex) {
      throw raise(RuntimeException.class, ex);
    } catch (ArityException ex) {
      throw raise(RuntimeException.class, ex);
    } catch (UnsupportedMessageException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> type, Throwable t) throws E {
    throw (E)t;
  }

  @ExportLibrary(InteropLibrary.class)
  static final class HoleInAtom implements TruffleObject {
    private final Atom atom;
    private final int index;
    private final Object empty;

    HoleInAtom(Atom atom, int index, Object empty) {
      this.atom = atom;
      this.index = index;
      this.empty = empty;
    }

    @ExportMessage
    Object invokeMember(String name, Object[] args, @CachedLibrary("this") InteropLibrary iop) throws UnsupportedMessageException {
      if ("set".equals(name)) {
        return execute(args, iop);
      } else {
        throw UnsupportedMessageException.create();
      }
    }

    @ExportMessage
    boolean isMemberInvocable(String name) {
      return "set".equals(name);
    }

    @ExportMessage boolean hasMembers() {
      return true;
    }

    @ExportMessage
    Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
      return new Array("set");
    }

    @ExportMessage
    Object execute(Object[] args, @CachedLibrary("this") InteropLibrary iop) {
      var prev = atom.getFields()[index];
      if (prev == empty) {
        atom.getFields()[index] = args[0];
      }
      return EnsoContext.get(iop).getBuiltins().nothing();
    }

    @ExportMessage
    boolean isExecutable() {
      return true;
    }
  }
}
