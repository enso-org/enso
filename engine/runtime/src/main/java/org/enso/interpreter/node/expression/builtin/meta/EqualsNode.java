package org.enso.interpreter.node.expression.builtin.meta;

import com.ibm.icu.text.Normalizer;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigInteger;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Arrays;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.ordering.HasCustomComparatorNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.atom.StructsLibrary;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Comparable",
    name = "equals_builtin",
    description = """
      Compares self with other object and returns True iff `self` is exactly the same as
      the other object, including all its transitively accessible properties or fields,
      False otherwise.
      
      Can handle arbitrary objects, including all foreign objects.
      
      Does not throw dataflow errors or panics.
      
      Note that this is different than `Meta.is_same_object`, which checks whether two
      references point to the same object on the heap.
      """
)
@GenerateUncached
public abstract class EqualsNode extends Node {

  public static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  public abstract boolean execute(@AcceptsError Object left, @AcceptsError Object right);

  /**
   * Primitive values
   **/


  @Specialization
  boolean equalsBoolBool(boolean self, boolean other) {
    return self == other;
  }

  @Specialization
  boolean equalsBoolDouble(boolean self, double other) {
    return false;
  }

  @Specialization
  boolean equalsBoolLong(boolean self, long other) {
    return false;
  }

  @Specialization
  boolean equalsBoolBigInt(boolean self, EnsoBigInteger other) {
    return false;
  }

  @Specialization
  boolean equalsBoolText(boolean self, Text other) {
    return false;
  }

  @Specialization
  boolean equalsByteByte(byte self, byte other) {
    return self == other;
  }

  @Specialization
  boolean equalsLongLong(long self, long other) {
    return self == other;
  }

  @Specialization
  boolean equalsLongBool(long self, boolean other) {
    return false;
  }

  @Specialization
  boolean equalsLongInt(long self, int other) {
    return self == (long) other;
  }

  @Specialization
  boolean equalsLongDouble(long self, double other) {
    return (double) self == other;
  }

  @Specialization
  boolean equalsLongText(long self, Text other) {
    return false;
  }

  @Specialization
  boolean equalsDoubleDouble(double self, double other) {
    if (Double.isNaN(self) || Double.isNaN(other)) {
      return false;
    } else {
      return self == other;
    }
  }

  @Specialization
  boolean equalsDoubleLong(double self, long other) {
    return self == (double) other;
  }

  @Specialization
  boolean equalsDoubleBool(double self, boolean other) {
    return false;
  }

  @Specialization
  boolean equalsDoubleInt(double self, int other) {
    return self == (double) other;
  }

  @Specialization
  @TruffleBoundary
  boolean equalsDoubleBigInt(double self, EnsoBigInteger other) {
    return self == other.doubleValue();
  }

  @Specialization
  boolean equalsDoubleText(double self, Text other) {
    return false;
  }

  @Specialization
  boolean equalsIntInt(int self, int other) {
    return self == other;
  }

  @Specialization
  boolean equalsIntLong(int self, long other) {
    return (long) self == other;
  }

  @Specialization
  boolean equalsIntDouble(int self, double other) {
    return (double) self == other;
  }

  @Specialization
  @TruffleBoundary
  boolean equalsBigIntBigInt(EnsoBigInteger self, EnsoBigInteger otherBigInt) {
    return self.equals(otherBigInt);
  }

  @Specialization
  @TruffleBoundary
  boolean equalsBitIntDouble(EnsoBigInteger self, double other) {
    return self.doubleValue() == other;
  }

  @Specialization
  @TruffleBoundary
  boolean equalsBigIntLong(EnsoBigInteger self, long other) {
    if (BigIntegerOps.fitsInLong(self.getValue())) {
      return self.getValue().compareTo(BigInteger.valueOf(other)) == 0;
    } else {
      return false;
    }
  }

  @Specialization
  boolean equalsBigIntBool(EnsoBigInteger self, boolean other) {
    return false;
  }

  @Specialization
  boolean equalsBigIntText(EnsoBigInteger self, Text other) {
    return false;
  }

  @Specialization
  @TruffleBoundary
  boolean equalsLongBigInt(long self, EnsoBigInteger other) {
    if (BigIntegerOps.fitsInLong(other.getValue())) {
      return BigInteger.valueOf(self).compareTo(other.getValue()) == 0;
    } else {
      return false;
    }
  }

  @Specialization(guards = {
    "selfText.is_normalized()",
    "otherText.is_normalized()"
  })
  boolean equalsTextText(Text selfText, Text otherText) {
    return selfText.toString().compareTo(otherText.toString()) == 0;
  }

  @Specialization
  boolean equalsTextBool(Text self, boolean other) {
    return false;
  }

  @Specialization
  boolean equalsTextLong(Text selfText, long otherLong) {
    return false;
  }

  @Specialization
  boolean equalsTextDouble(Text selfText, double otherDouble) {
    return false;
  }

  @Specialization
  boolean equalsTextBigInt(Text self, EnsoBigInteger other) {
    return false;
  }

  @Specialization(guards = "isPrimitive(self) || isPrimitive(other)")
  boolean equalsDifferent(Object self, Object other) {
    return false;
  }
  
  @Specialization(guards = "nonPrimitive(self, other)")
  boolean equalsComplex(
    Object self, Object other,
    @Cached EqualsComplexNode complex
  ) {
    return complex.execute(self, other);
  }
  
  static boolean nonPrimitive(Object a, Object b) {
    return !isPrimitive(a) && !isPrimitive(b);
  }

  /**
   * Return true iff object is a primitive value used in some specializations
   * guard. By primitive value we mean any value that can be present in Enso, so,
   * for example, not Integer, as that cannot be present in Enso.
   * All the primitive types should be handled in their corresponding specializations.
   * See {@link org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode}.
   */
  static boolean isPrimitive(Object object) {
    return object instanceof Boolean ||
        object instanceof Long ||
        object instanceof Double ||
        object instanceof EnsoBigInteger ||
        object instanceof Text;
  }
}
