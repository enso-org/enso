package org.enso.interpreter.node.expression.builtin.error;

import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
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

  abstract Stateful execute(Object _this, Object payload);

  @Specialization
  Stateful doAtom(
      Object _this, Atom payload, @CachedLibrary(limit = "5") InteropLibrary interopLibrary) {
    Builtins builtins = Context.get(this).getBuiltins();
    if (payload.getConstructor() == builtins.caughtPanic()) {
      // Note [Original Exception Type]
      Object originalException = payload.getFields()[1];
      if (interopLibrary.isException(originalException)) {
        try {
          throw interopLibrary.throwException(originalException);
        } catch (UnsupportedMessageException e) {
          /* We cannot rethrow the original exception, so we throw a panic with
           * the given payload, marked as originating from here, as we cannot
           * reconstruct a better location.
           *
           * This situation should never arise, based on the contract of
           * `throwException` and `isException`.
           */
          throw new PanicException(payload.getFields()[0], this);
        }
      } else {
        throw new PanicException(
            builtins
                .error()
                .makeTypeError("Exception", originalException, "internal_original_exception"),
            this);
      }
    } else {
      throw new PanicException(payload, this);
    }
  }

  @Fallback
  Stateful doFallback(
      Object _this, Object payload, @CachedLibrary(limit = "5") InteropLibrary interopLibrary) {
    if (interopLibrary.isException(payload)) {
      try {
        throw interopLibrary.throwException(payload);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException("Impossible, `isException` returned true for `payload`.");
      }
    } else {
      throw new PanicException(payload, this);
    }
  }
}

/* Note [Original Exception Type]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * We can assume that the exception stored in a `Caught_Panic` atom is a
 * subclass of a `AbstractTruffleException`, because the only place which
 * constructs `Caught_Panic` puts a `PanicException` there or an
 * `AbstractTruffleException` directly.
 */
