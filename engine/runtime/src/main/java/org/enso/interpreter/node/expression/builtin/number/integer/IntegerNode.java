package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.number.utils.ToEnsoNumberNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

/** A base class for nodes that operate on Enso Integers (e.g. either {@code long} or {@link EnsoBigInteger}). Contains helper
 * methods that can be used from subclasses of either {@link Unary} or {@link Binary}
 * variant of this class.
 */
public sealed abstract class IntegerNode extends Node permits IntegerNode.Unary, IntegerNode.Binary {
  @Child private ToEnsoNumberNode toEnsoNumberNode;
  @Child private InteropLibrary iop;

  private IntegerNode() {}

  @TruffleBoundary
  final PanicException throwTypeErrorIfNotInt(Object self, Object that) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var intType = builtins.number().getInteger();
    var selfType = TypesLibrary.getUncached().getType(self);
    if (selfType != intType) {
      return new PanicException(builtins.error().makeTypeError(intType, self, "self"), this);
    } else {
      return new PanicException(builtins.error().makeTypeError(intType, that, "that"), this);
    }
  }

  @TruffleBoundary
  final PanicException throwTypeErrorIfNotInt(Object self) {
    var builtins = EnsoContext.get(this).getBuiltins();
    var intType = builtins.number().getInteger();
    return new PanicException(builtins.error().makeTypeError(intType, self, "self"), this);
  }

  final Object toEnsoNumberOrNull(Object obj) {
    return toEnsoNumberOrNull(obj, true);
  }

  final Object toEnsoNumberOrNull(Object obj, boolean acceptDouble) {
    if (obj instanceof Long) {
      return obj;
    }
    if (obj instanceof EnsoBigInteger) {
      return obj;
    }
    if (acceptDouble && obj instanceof Double) {
      return obj;
    }
    if (obj instanceof TruffleObject) {
      try {
        if (iop == null) {
          CompilerDirectives.transferToInterpreterAndInvalidate();
          iop = insert(InteropLibrary.getFactory().createDispatched(3));
        }
        if (iop.isNumber(obj)) {
          if (iop.fitsInLong(obj)) {
            return iop.asLong(obj);
          } else if (acceptDouble && iop.fitsInDouble(obj)) {
            return iop.asDouble(obj);
          } else if (iop.fitsInBigInteger(obj)) {
            return toEnsoNumberNode().execute(iop.asBigInteger(obj));
          }
        }
      } catch (UnsupportedMessageException ex) {
        // no conversion
      }
    }
    return null;
  }

  final ToEnsoNumberNode toEnsoNumberNode() {
    if (toEnsoNumberNode == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      toEnsoNumberNode = insert(ToEnsoNumberNode.create());
    }
    return toEnsoNumberNode;
  }

  /**
   * Node operating on a single Enso Integer (e.g. either {@code long} or {@link EnsoBigInteger}) argument.
   */
  public non-sealed abstract static class Unary extends IntegerNode {
    public final Object execute(Object own) {
      var ensoSelf = toEnsoNumberOrNull(own, false);
      if (ensoSelf == null) {
          throw throwTypeErrorIfNotInt(own);
      }
      return executeUnary(ensoSelf);
    }

    abstract Object executeUnary(Object self);
  }

  /**
   * Node operating on a tow Enso Integers (e.g. either {@code long} or {@link EnsoBigInteger}) arguments.
   */
  public non-sealed abstract static class Binary extends IntegerNode {
    public final Object execute(Object own, Object that) {
      var ensoSelf = toEnsoNumberOrNull(own, false);
      var ensoThat = toEnsoNumberOrNull(that);
      if (ensoSelf == null || ensoThat == null) {
          throw throwTypeErrorIfNotInt(own, that);
      }
      return executeBinary(ensoSelf, ensoThat);
    }

    abstract Object executeBinary(Object self, Object that);
  }
}
