package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoMultiValue;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
import org.enso.polyglot.common_utils.Core_Text_Utils;

@GenerateUncached
@ReportPolymorphism
abstract class EqualsSimpleNode extends Node {

  static EqualsSimpleNode build() {
    return EqualsSimpleNodeGen.create();
  }

  static EqualsSimpleNode getUncached() {
    return EqualsSimpleNodeGen.getUncached();
  }

  abstract EqualsAndInfo execute(VirtualFrame frame, Object self, Object right);

  @Specialization
  EqualsAndInfo equalsBoolBool(boolean self, boolean other) {
    return EqualsAndInfo.valueOf(self == other);
  }

  @Specialization
  EqualsAndInfo equalsBoolDouble(boolean self, double other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsBoolLong(boolean self, long other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsBoolBigInt(boolean self, EnsoBigInteger other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsBoolText(boolean self, Text other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsBoolInterop(
      boolean self,
      Object other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    try {
      return EqualsAndInfo.valueOf(self == interop.asBoolean(other));
    } catch (UnsupportedMessageException ex) {
      return EqualsAndInfo.FALSE;
    }
  }

  @Specialization
  EqualsAndInfo equalsInteropBool(
      TruffleObject self,
      boolean other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    try {
      return EqualsAndInfo.valueOf(other == interop.asBoolean(self));
    } catch (UnsupportedMessageException ex) {
      return EqualsAndInfo.FALSE;
    }
  }

  @Specialization
  EqualsAndInfo equalsLongLong(long self, long other) {
    return EqualsAndInfo.valueOf(self == other);
  }

  @Specialization
  EqualsAndInfo equalsLongBool(long self, boolean other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsLongDouble(long self, double other) {
    return EqualsAndInfo.valueOf((double) self == other);
  }

  @Specialization
  EqualsAndInfo equalsLongText(long self, Text other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  @TruffleBoundary(allowInlining = true)
  EqualsAndInfo equalsLongBigInt(long self, EnsoBigInteger other) {
    if (BigIntegerOps.fitsInLong(other.getValue())) {
      return EqualsAndInfo.valueOf(BigInteger.valueOf(self).compareTo(other.getValue()) == 0);
    } else {
      return EqualsAndInfo.FALSE;
    }
  }

  @Specialization
  EqualsAndInfo equalsLongInterop(
      long self,
      Object other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    try {
      return EqualsAndInfo.valueOf(self == interop.asLong(other));
    } catch (UnsupportedMessageException ex) {
      return EqualsAndInfo.FALSE;
    }
  }

  @Specialization
  EqualsAndInfo equalsDoubleDouble(double self, double other) {
    if (Double.isNaN(self) || Double.isNaN(other)) {
      return EqualsAndInfo.FALSE;
    } else {
      return EqualsAndInfo.valueOf(self == other);
    }
  }

  @Specialization
  EqualsAndInfo equalsDoubleLong(double self, long other) {
    return EqualsAndInfo.valueOf(self == (double) other);
  }

  @Specialization
  EqualsAndInfo equalsDoubleBool(double self, boolean other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsDoubleText(double self, Text other) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsDoubleInterop(
      double self,
      Object other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    try {
      if (iop.fitsInDouble(other)) {
        return EqualsAndInfo.valueOf(self == iop.asDouble(other));
      }
      if (iop.fitsInBigInteger(other)) {
        if (Double.isNaN(self)) {
          return EqualsAndInfo.FALSE;
        }
        if (Double.isInfinite(self)) {
          return EqualsAndInfo.FALSE;
        }
        return EqualsAndInfo.valueOf(equalsDoubleBigInteger(self, asBigInteger(iop, other)));
      } else {
        return EqualsAndInfo.FALSE;
      }
    } catch (UnsupportedMessageException ex) {
      return EqualsAndInfo.FALSE;
    }
  }

  @TruffleBoundary
  private static boolean equalsDoubleBigInteger(double self, BigInteger big) {
    var selfDecimal = new BigDecimal(self);
    var otherDecimal = new BigDecimal(big);
    return selfDecimal.equals(otherDecimal);
  }

  @Specialization(guards = {"isBigInteger(iop, self)", "isBigInteger(iop, other)"})
  @TruffleBoundary(allowInlining = true)
  EqualsAndInfo other(
      Object self,
      Object other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    return EqualsAndInfo.valueOf(asBigInteger(iop, self).equals(asBigInteger(iop, other)));
  }

  @Specialization(guards = "isBigInteger(iop, self)")
  EqualsAndInfo equalsBigIntDouble(
      Object self,
      double other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    if (Double.isNaN(other)) {
      return EqualsAndInfo.FALSE;
    }
    if (Double.isInfinite(other)) {
      return EqualsAndInfo.FALSE;
    }
    return EqualsAndInfo.valueOf(equalsDoubleBigInteger(other, asBigInteger(iop, self)));
  }

  @Specialization(guards = "isBigInteger(iop, self)")
  @TruffleBoundary(allowInlining = true)
  EqualsAndInfo equalsBigIntLong(
      Object self, long other, @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    var v = asBigInteger(iop, self);
    if (BigIntegerOps.fitsInLong(v)) {
      return EqualsAndInfo.valueOf(v.compareTo(BigInteger.valueOf(other)) == 0);
    } else {
      return EqualsAndInfo.FALSE;
    }
  }

  @Specialization
  EqualsAndInfo equalsBigIntText(EnsoBigInteger self, Text other) {
    return EqualsAndInfo.FALSE;
  }

  @TruffleBoundary
  @Specialization(guards = {"isBigInteger(iop, self)", "!isPrimitiveValue(other)"})
  EqualsAndInfo equalsBigIntInterop(
      Object self,
      Object other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary iop) {
    try {
      var otherBigInteger = InteropLibrary.getUncached().asBigInteger(other);
      return EqualsAndInfo.valueOf(asBigInteger(iop, self).equals(otherBigInteger));
    } catch (UnsupportedMessageException ex) {
      return EqualsAndInfo.FALSE;
    }
  }

  @Specialization(guards = {"selfText.is_normalized()", "otherText.is_normalized()"})
  EqualsAndInfo equalsTextText(Text selfText, Text otherText) {
    return EqualsAndInfo.valueOf(selfText.toString().compareTo(otherText.toString()) == 0);
  }

  @Specialization
  EqualsAndInfo equalsTextLong(Text selfText, long otherLong) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsTextDouble(Text selfText, double otherDouble) {
    return EqualsAndInfo.FALSE;
  }

  @Specialization
  EqualsAndInfo equalsTextBigInt(Text self, EnsoBigInteger other) {
    return EqualsAndInfo.FALSE;
  }

  /**
   * Compares interop string with other object. If the other object doesn't support conversion to
   * String, it is not equal. Otherwise the two strings are compared according to the
   * lexicographical order, handling Unicode normalization. See {@code Text_Utils.compare_to}.
   */
  @Specialization(
      guards = {"selfInterop.isString(selfString)"},
      limit = "3")
  EqualsAndInfo equalsStrings(
      Object selfString,
      Object otherString,
      @CachedLibrary("selfString") InteropLibrary selfInterop,
      @CachedLibrary("otherString") InteropLibrary otherInterop) {
    String selfJavaString;
    String otherJavaString;
    try {
      selfJavaString = selfInterop.asString(selfString);
      otherJavaString = otherInterop.asString(otherString);
    } catch (UnsupportedMessageException e) {
      return EqualsAndInfo.FALSE;
    }
    return EqualsAndInfo.valueOf(compareStrings(selfJavaString, otherJavaString));
  }

  @TruffleBoundary
  private static boolean compareStrings(String selfJavaString, String otherJavaString) {
    return Core_Text_Utils.equals(selfJavaString, otherJavaString);
  }

  @Specialization(guards = "isPrimitive(self, interop) != isPrimitive(other, interop)")
  EqualsAndInfo equalsDifferent(
      Object self,
      Object other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    return EqualsAndInfo.FALSE;
  }

  /** Equals for Atoms and AtomConstructors */
  @Specialization
  EqualsAndInfo equalsAtomConstructors(AtomConstructor self, AtomConstructor other) {
    return EqualsAndInfo.valueOf(self == other);
  }

  @Specialization
  EqualsAndInfo equalsAtoms(
      VirtualFrame frame,
      Atom self,
      Atom other,
      @Cached EqualsAtomNode equalsAtomNode,
      @Shared("isSameObjectNode") @Cached IsSameObjectNode isSameObjectNode) {
    if (isSameObjectNode.execute(self, other)) {
      return EqualsAndInfo.TRUE;
    } else {
      return equalsAtomNode.execute(frame, self, other);
    }
  }

  @Specialization
  EqualsAndInfo equalsReverseLong(
      VirtualFrame frame,
      TruffleObject self,
      long other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("reverse") @Cached EqualsSimpleNode reverse) {
    return reverse.execute(frame, other, self);
  }

  @Specialization
  EqualsAndInfo equalsReverseDouble(
      VirtualFrame frame,
      TruffleObject self,
      double other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("reverse") @Cached EqualsSimpleNode reverse) {
    return reverse.execute(frame, other, self);
  }

  @Specialization
  EqualsAndInfo equalsReverseBigInt(
      VirtualFrame frame,
      TruffleObject self,
      EnsoBigInteger other,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("reverse") @Cached EqualsSimpleNode reverse) {
    return reverse.execute(frame, other, self);
  }

  @Specialization(guards = "isNotPrimitive(self, other, interop, warnings)")
  EqualsAndInfo equalsComplex(
      VirtualFrame frame,
      Object self,
      Object other,
      @Cached EqualsComplexNode equalsComplex,
      @Shared("isSameObjectNode") @Cached IsSameObjectNode isSameObjectNode,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "5") WarningsLibrary warnings) {
    if (isSameObjectNode.execute(self, other)) {
      return EqualsAndInfo.TRUE;
    } else {
      return equalsComplex.execute(frame, self, other);
    }
  }

  static boolean isNotPrimitive(
      Object a, Object b, InteropLibrary interop, WarningsLibrary warnings) {
    if (a instanceof AtomConstructor && b instanceof AtomConstructor) {
      return false;
    }
    if (a instanceof Atom && b instanceof Atom) {
      return false;
    }
    if (warnings.hasWarnings(a) || warnings.hasWarnings(b)) {
      return true;
    }
    if (a instanceof EnsoMultiValue || b instanceof EnsoMultiValue) {
      return true;
    }
    return !isPrimitive(a, interop) && !isPrimitive(b, interop);
  }

  /**
   * Return true iff object is a primitive value used in some specializations guard. By primitive
   * value we mean any value that can be present in Enso, so, for example, not Integer, as that
   * cannot be present in Enso. All the primitive types should be handled in their corresponding
   * specializations. See {@link
   * org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode}.
   */
  static boolean isPrimitive(Object object, InteropLibrary interop) {
    return isPrimitiveValue(object)
        || object instanceof EnsoBigInteger
        || object instanceof Text
        || interop.isString(object)
        || interop.isNumber(object)
        || interop.isBoolean(object);
  }

  static boolean isPrimitiveValue(Object object) {
    return object instanceof Boolean || object instanceof Long || object instanceof Double;
  }

  static boolean isBigInteger(InteropLibrary iop, Object v) {
    return switch (v) {
      case EnsoBigInteger b -> true;
      case EnsoObject no -> false;
      case Long ok -> true;
      case Double doesNotFit -> false;
      default -> iop.fitsInBigInteger(v);
    };
  }

  BigInteger asBigInteger(InteropLibrary iop, Object v) {
    if (v instanceof EnsoBigInteger big) {
      return big.getValue();
    } else {
      try {
        return iop.asBigInteger(v);
      } catch (UnsupportedMessageException ex) {
        throw EnsoContext.get(this).raiseAssertionPanic(this, "Expecting BigInteger", ex);
      }
    }
  }
}
