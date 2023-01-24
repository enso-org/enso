package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(
    type = "Panic",
    name = "throw",
    description = "Throws a new Panic with given payload.")
public abstract class ThrowPanicNode extends Node {
  static ThrowPanicNode build() {
    return ThrowPanicNodeGen.create();
  }

  abstract Object execute(Object payload);

  EnsoContext getContext() {
    return EnsoContext.get(this);
  }

  @Specialization(
      guards = {
        "payload.getConstructor().getType() == getContext().getBuiltins().caughtPanic().getType()"
      })
  Object doCaughtPanic(
      Atom payload,
      @CachedLibrary(limit = "5") InteropLibrary interopLibrary,
      @CachedLibrary(limit = "5") StructsLibrary structs,
      @Cached BranchProfile typeErrorProfile) {
    // Note [Original Exception Type]
    Object originalException = structs.getField(payload, 1);
    if (interopLibrary.isException(originalException)) {
      try {
        throw interopLibrary.throwException(originalException);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(
            "Impossible, `isException` returned true for `originalException`.");
      }
    } else {
      typeErrorProfile.enter();
      Builtins builtins = EnsoContext.get(this).getBuiltins();
      throw new PanicException(
          builtins
              .error()
              .makeTypeError("Exception", originalException, "internal_original_exception"),
          this);
    }
  }

  @Specialization(guards = "interopLibrary.isException(payload)")
  Object doOtherException(
      Object payload, @CachedLibrary(limit = "5") InteropLibrary interopLibrary) {
    try {
      throw interopLibrary.throwException(payload);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `isException` returned true for `payload`.");
    }
  }

  @Fallback
  Object doFallback(Object payload) {
    throw new PanicException(payload, this);
  }
}

/* Note [Original Exception Type]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * We can assume that the exception stored in a `Caught_Panic` atom is a
 * subclass of a `AbstractTruffleException`, because the only place which
 * constructs `Caught_Panic` puts a `PanicException` there or an
 * `AbstractTruffleException` directly.
 */
