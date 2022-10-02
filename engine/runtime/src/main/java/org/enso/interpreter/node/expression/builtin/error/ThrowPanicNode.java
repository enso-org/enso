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
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.Stateful;

@BuiltinMethod(
    type = "Panic",
    name = "throw",
    description = "Throws a new Panic with given payload.")
public abstract class ThrowPanicNode extends Node {
  static ThrowPanicNode build() {
    return ThrowPanicNodeGen.create();
  }

  abstract Stateful execute(Object payload);

  Context getContext() {
    return Context.get(this);
  }

  @Specialization(
      guards = {
        "payload.getConstructor().getType() == getContext().getBuiltins().caughtPanic().getType()"
      })
  Stateful doCaughtPanic(
      Atom payload,
      @CachedLibrary(limit = "5") InteropLibrary interopLibrary,
      @Cached BranchProfile typeErrorProfile) {
    // Note [Original Exception Type]
    Object originalException = payload.getFields()[1];
    if (interopLibrary.isException(originalException)) {
      try {
        throw interopLibrary.throwException(originalException);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(
            "Impossible, `isException` returned true for `originalException`.");
      }
    } else {
      typeErrorProfile.enter();
      Builtins builtins = Context.get(this).getBuiltins();
      throw new PanicException(
          builtins
              .error()
              .makeTypeError("Exception", originalException, "internal_original_exception"),
          this);
    }
  }

  @Specialization(guards = "interopLibrary.isException(payload)")
  Stateful doOtherException(
      Object payload, @CachedLibrary(limit = "5") InteropLibrary interopLibrary) {
    try {
      throw interopLibrary.throwException(payload);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Impossible, `isException` returned true for `payload`.");
    }
  }

  @Fallback
  Stateful doFallback(Object payload) {
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
