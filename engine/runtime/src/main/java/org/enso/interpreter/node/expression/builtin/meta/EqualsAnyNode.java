package org.enso.interpreter.node.expression.builtin.meta;

import com.ibm.icu.text.Normalizer;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.profiles.LoopConditionProfile;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Map;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNodeGen.InvokeEqualsNodeGen;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.state.State;
import org.enso.polyglot.MethodNames;

@BuiltinMethod(
    type = "Any",
    name = "==",
    description = "Implementation of Any.=="
)
@GenerateUncached
public abstract class EqualsAnyNode extends Node {

  protected static String EQUALS_MEMBER_NAME = MethodNames.Function.EQUALS;

  public static EqualsAnyNode build() {
    return EqualsAnyNodeGen.create();
  }
  public abstract boolean execute(@AcceptsError Object self, @AcceptsError Object right);

  /** Primitive values **/


  @Specialization
  boolean equalsBoolean(boolean self, boolean other) {
    return self == other;
  }

  @Specialization
  boolean equalsLong(long self, long other) {
    return self == other;
  }

  @Specialization
  boolean equalsDouble(double self, double other) {
    return self == other;
  }

  @Specialization
  boolean equalsLongDouble(long self, double other) {
    return (double) self == other;
  }

  @Specialization
  boolean equalsDoubleLong(double self, long other) {
    return self == (double) other;
  }

  @Specialization
  boolean equalsIntLong(int self, long other) {
    return (long) self == other;
  }

  @Specialization
  boolean equalsLongInt(long self, int other) {
    return self == (long) other;
  }

  @Specialization
  boolean equalsIntDouble(int self, double other) {
    return (double) self == other;
  }

  @Specialization
  boolean equalsDoubleInt(double self, int other) {
    return self == (double) other;
  }

  @Specialization
  @TruffleBoundary
  boolean equalsBigInt(EnsoBigInteger self, EnsoBigInteger otherBigInt) {
    return self.equals(otherBigInt);
  }

  @Specialization
  @TruffleBoundary
  boolean equalsBitIntDouble(EnsoBigInteger self, double other) {
    return self.doubleValue() == other;
  }

  @Specialization
  @TruffleBoundary
  boolean equalsDoubleBigInt(double self, EnsoBigInteger other) {
    return self == other.doubleValue();
  }

  /** Enso specific types **/

  @Specialization
  boolean equalsUnresolvedSymbols(UnresolvedSymbol self, UnresolvedSymbol otherSymbol,
      @Cached EqualsAnyNode equalsNode) {
    return self.getName().equals(otherSymbol.getName())
        && equalsNode.execute(self.getScope(), otherSymbol.getScope());
  }

  @Specialization
  boolean equalsUnresolvedConversion(UnresolvedConversion selfConversion, UnresolvedConversion otherConversion,
      @Cached EqualsAnyNode equalsNode) {
    return equalsNode.execute(selfConversion.getScope(), otherConversion.getScope());
  }

  /**
   * If one of the objects has warnings attached, just treat it as an object without any
   * warnings.
   */
  @Specialization(guards = {
      "selfWarnLib.hasWarnings(selfWithWarnings) || otherWarnLib.hasWarnings(otherWithWarnings)"
  }, limit = "3")
  boolean equalsWithWarnings(Object selfWithWarnings, Object otherWithWarnings,
      @CachedLibrary("selfWithWarnings") WarningsLibrary selfWarnLib,
      @CachedLibrary("otherWithWarnings") WarningsLibrary otherWarnLib,
      @Cached EqualsAnyNode equalsNode
  ) {
    try {
      Object self =
          selfWarnLib.hasWarnings(selfWithWarnings) ? selfWarnLib.removeWarnings(selfWithWarnings)
              : selfWithWarnings;
      Object other =
          otherWarnLib.hasWarnings(otherWithWarnings) ? otherWarnLib.removeWarnings(otherWithWarnings)
              : otherWithWarnings;
      return equalsNode.execute(self, other);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  /** Interop libraries **/

  @Specialization(guards = {
      "selfInterop.isNull(selfNull)",
      "otherInterop.isNull(otherNull)"
  }, limit = "3")
  boolean equalsNull(
      Object selfNull, Object otherNull,
      @CachedLibrary("selfNull") InteropLibrary selfInterop,
      @CachedLibrary("otherNull") InteropLibrary otherInterop
  ) {
    return true;
  }

  @Specialization(guards = {
      "selfInterop.isBoolean(selfBoolean)",
      "otherInterop.isBoolean(otherBoolean)"
  }, limit = "3")
  boolean equalsBooleanInterop(
      Object selfBoolean,
      Object otherBoolean,
      @CachedLibrary("selfBoolean") InteropLibrary selfInterop,
      @CachedLibrary("otherBoolean") InteropLibrary otherInterop
  ) {
    try {
      return selfInterop.asBoolean(selfBoolean) == otherInterop.asBoolean(otherBoolean);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "!selfInterop.isDate(selfTimeZone)",
      "!selfInterop.isTime(selfTimeZone)",
      "selfInterop.isTimeZone(selfTimeZone)",
      "!otherInterop.isDate(otherTimeZone)",
      "!otherInterop.isTime(otherTimeZone)",
      "otherInterop.isTimeZone(otherTimeZone)"
  }, limit = "3")
  boolean equalsTimeZones(Object selfTimeZone, Object otherTimeZone,
      @CachedLibrary("selfTimeZone") InteropLibrary selfInterop,
      @CachedLibrary("otherTimeZone") InteropLibrary otherInterop) {
    try {
      return selfInterop.asTimeZone(selfTimeZone).equals(
          otherInterop.asTimeZone(otherTimeZone)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @TruffleBoundary
  @Specialization(guards = {
      "selfInterop.isDate(selfZonedDateTime)",
      "selfInterop.isTime(selfZonedDateTime)",
      "selfInterop.isTimeZone(selfZonedDateTime)",
      "otherInterop.isDate(otherZonedDateTime)",
      "otherInterop.isTime(otherZonedDateTime)",
      "otherInterop.isTimeZone(otherZonedDateTime)"
  }, limit = "3")
  boolean equalsZonedDateTimes(Object selfZonedDateTime, Object otherZonedDateTime,
      @CachedLibrary("selfZonedDateTime") InteropLibrary selfInterop,
      @CachedLibrary("otherZonedDateTime") InteropLibrary otherInterop) {
    try {
      var self = ZonedDateTime.of(
          selfInterop.asDate(selfZonedDateTime),
          selfInterop.asTime(selfZonedDateTime),
          selfInterop.asTimeZone(selfZonedDateTime)
      );
      var other = ZonedDateTime.of(
          otherInterop.asDate(otherZonedDateTime),
          otherInterop.asTime(otherZonedDateTime),
          otherInterop.asTimeZone(otherZonedDateTime)
      );
      return self.isEqual(other);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "selfInterop.isDate(selfDateTime)",
      "selfInterop.isTime(selfDateTime)",
      "!selfInterop.isTimeZone(selfDateTime)",
      "otherInterop.isDate(otherDateTime)",
      "otherInterop.isTime(otherDateTime)",
      "!otherInterop.isTimeZone(otherDateTime)"
  }, limit = "3")
  boolean equalsDateTimes(Object selfDateTime, Object otherDateTime,
      @CachedLibrary("selfDateTime") InteropLibrary selfInterop,
      @CachedLibrary("otherDateTime") InteropLibrary otherInterop) {
    try {
      var self = LocalDateTime.of(
          selfInterop.asDate(selfDateTime),
          selfInterop.asTime(selfDateTime)
      );
      var other = LocalDateTime.of(
          otherInterop.asDate(otherDateTime),
          otherInterop.asTime(otherDateTime)
      );
      return self.isEqual(other);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "selfInterop.isDate(selfDate)",
      "!selfInterop.isTime(selfDate)",
      "!selfInterop.isTimeZone(selfDate)",
      "otherInterop.isDate(otherDate)",
      "!otherInterop.isTime(otherDate)",
      "!otherInterop.isTimeZone(otherDate)"
  }, limit = "3")
  boolean equalsDates(Object selfDate, Object otherDate,
      @CachedLibrary("selfDate") InteropLibrary selfInterop,
      @CachedLibrary("otherDate") InteropLibrary otherInterop) {
    try {
      return selfInterop.asDate(selfDate).isEqual(
          otherInterop.asDate(otherDate)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "!selfInterop.isDate(selfTime)",
      "selfInterop.isTime(selfTime)",
      "!selfInterop.isTimeZone(selfTime)",
      "!otherInterop.isDate(otherTime)",
      "otherInterop.isTime(otherTime)",
      "!otherInterop.isTimeZone(otherTime)"
  }, limit = "3")
  boolean equalsTimes(Object selfTime, Object otherTime,
      @CachedLibrary("selfTime") InteropLibrary selfInterop,
      @CachedLibrary("otherTime") InteropLibrary otherInterop) {
    try {
      return selfInterop.asTime(selfTime).equals(
          otherInterop.asTime(otherTime)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "selfInterop.isDuration(selfDuration)",
      "otherInterop.isDuration(otherDuration)"
  }, limit = "3")
  boolean equalsDuration(Object selfDuration, Object otherDuration,
      @CachedLibrary("selfDuration") InteropLibrary selfInterop,
      @CachedLibrary("otherDuration") InteropLibrary otherInterop) {
    try {
      return selfInterop.asDuration(selfDuration).equals(
          otherInterop.asDuration(otherDuration)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * Compares interop strings according to the lexicographical order, handling Unicode
   * normalization. See {@code Text_Utils.compare_to}.
   */
  @TruffleBoundary
  @Specialization(guards = {
      "selfInterop.isString(selfString)",
      "otherInterop.isString(otherString)"
  }, limit = "3")
  boolean equalsStrings(Object selfString, Object otherString,
      @CachedLibrary("selfString") InteropLibrary selfInterop,
      @CachedLibrary("otherString") InteropLibrary otherInterop) {
    String selfJavaString;
    String otherJavaString;
    try {
      selfJavaString = selfInterop.asString(selfString);
      otherJavaString = otherInterop.asString(otherString);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    return Normalizer.compare(
        selfJavaString,
        otherJavaString,
        Normalizer.FOLD_CASE_DEFAULT
    ) == 0;
  }

  @Specialization(guards = {
      "selfInterop.hasArrayElements(selfArray)",
      "otherInterop.hasArrayElements(otherArray)"
  }, limit = "3")
  boolean equalsArrays(Object selfArray, Object otherArray,
      @CachedLibrary("selfArray") InteropLibrary selfInterop,
      @CachedLibrary("otherArray") InteropLibrary otherInterop,
      @Cached EqualsAnyNode equalsNode
      ) {
    try {
      long selfSize = selfInterop.getArraySize(selfArray);
      if (selfSize != otherInterop.getArraySize(otherArray)) {
        return false;
      }
      for (long i = 0; i < selfSize; i++) {
        Object selfElem = selfInterop.readArrayElement(selfArray, i);
        Object otherElem = otherInterop.readArrayElement(otherArray, i);
        if (!equalsNode.execute(selfElem, otherElem)) {
          return false;
        }
      }
      return true;
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
  }

  /** Equals for Atoms and AtomConstructors */

  @Specialization
  boolean equalsAtomConstructors(AtomConstructor selfConstructor, AtomConstructor otherConstructor) {
    return selfConstructor == otherConstructor;
  }

  /**
   * How many {@link EqualsAnyNode} should be created for fields in specialization for atoms.
   */
  static final int equalsNodeCountForFields = 10;

  static EqualsAnyNode[] createEqualsNodes(int size) {
    EqualsAnyNode[] nodes = new EqualsAnyNode[size];
    Arrays.fill(nodes, EqualsAnyNode.build());
    return nodes;
  }

  @Specialization
  boolean equalsAtoms(Atom self, Atom other,
      @Cached LoopConditionProfile loopProfile,
      @Cached(value = "createEqualsNodes(equalsNodeCountForFields)", allowUncached = true) EqualsAnyNode[] fieldEqualsNodes,
      @Cached InvokeEqualsNode atomInvokeEqualsNode,
      @Cached ConditionProfile enoughEqualNodesForFieldsProfile,
      @Cached ConditionProfile constructorsNotEqualProfile) {
    if (atomOverridesEquals(self)) {
      return atomInvokeEqualsNode.execute(self, other);
    }

    if (constructorsNotEqualProfile.profile(
        self.getConstructor() != other.getConstructor()
    )) {
      return false;
    }
    assert self.getFields().length == other.getFields().length;

    int fieldsSize = self.getFields().length;
    if (enoughEqualNodesForFieldsProfile.profile(fieldsSize <= equalsNodeCountForFields)) {
      loopProfile.profileCounted(fieldsSize);
      for (int i = 0; loopProfile.inject(i < fieldsSize); i++) {
        if (!fieldEqualsNodes[i].execute(
            self.getFields()[i],
            other.getFields()[i]
        )) {
          return false;
        }
      }
    } else {
      return equalsAtomsFieldsUncached(self.getFields(), other.getFields());
    }
    return true;
  }

  @TruffleBoundary
  private static boolean equalsAtomsFieldsUncached(Object[] selfFields, Object[] otherFields) {
    assert selfFields.length == otherFields.length;
    for (int i = 0; i < selfFields.length; i++) {
      boolean areFieldsSame;
      if (selfFields[i] instanceof Atom selfFieldAtom
          && otherFields[i] instanceof Atom otherFieldAtom
          && atomOverridesEquals(selfFieldAtom)) {
        areFieldsSame = InvokeEqualsNode.getUncached().execute(selfFieldAtom, otherFieldAtom);
      } else {
        areFieldsSame = EqualsAnyNodeGen.getUncached().execute(selfFields[i], otherFields[i]);
      }
      if (!areFieldsSame) {
        return false;
      }
    }
    return true;
  }

  /**
   * Helper node for invoking `==` method on atoms, that override this method.
   */
  @GenerateUncached
  static abstract class InvokeEqualsNode extends Node {
    static InvokeEqualsNode getUncached() {
      return InvokeEqualsNodeGen.getUncached();
    }

    static InvokeEqualsNode build() {
      return InvokeEqualsNodeGen.create();
    }

    abstract boolean execute(Atom selfAtom, Atom otherAtom);

    @Specialization(guards = "cachedSelfAtomCtor == selfAtom.getConstructor()")
    boolean invokeEqualsCachedAtomCtor(Atom selfAtom, Atom otherAtom,
        @Cached("selfAtom.getConstructor()") AtomConstructor cachedSelfAtomCtor,
        @Cached("getEqualsMethod(cachedSelfAtomCtor)") Function equalsMethod,
        @Cached ExecuteCallNode executeCallNode,
        @CachedLibrary(limit = "3") InteropLibrary interop) {
      assert atomOverridesEquals(selfAtom);
      Object ret = executeCallNode.executeCall(
          equalsMethod,
          null,
          State.create(EnsoContext.get(this)),
          new Object[]{selfAtom, otherAtom}
      );
      try {
        return interop.asBoolean(ret);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(e);
      }
    }

    @TruffleBoundary
    @Specialization(replaces = "invokeEqualsCachedAtomCtor")
    boolean invokeEqualsUncached(Atom selfAtom, Atom otherAtom,
        @Cached ExecuteCallNode executeCallNode) {
      Function equalsMethod = getEqualsMethod(selfAtom.getConstructor());
      Object ret = executeCallNode.executeCall(
          equalsMethod,
          null,
          State.create(EnsoContext.get(this)),
          new Object[]{selfAtom, otherAtom}
      );
      assert InteropLibrary.getUncached().isBoolean(ret);
      try {
        return InteropLibrary.getUncached().asBoolean(ret);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(e);
      }
    }

    @TruffleBoundary
    static Function getEqualsMethod(AtomConstructor atomConstructor) {
      Type atomType = atomConstructor.getType();
      Function equalsFunction = atomConstructor
          .getDefinitionScope()
          .getMethods()
          .get(atomType)
          .get("==");
      assert equalsFunction != null;
      return equalsFunction;
    }
  }

  /**
   * Returns true if the given atom overrides `==` operator.
   */
  @TruffleBoundary
  private static boolean atomOverridesEquals(Atom atom) {
    var atomType = atom.getConstructor().getType();
    Map<String, Function> methodsOnType = atom
        .getConstructor()
        .getDefinitionScope()
        .getMethods()
        .get(atomType);
    if (methodsOnType != null) {
      return methodsOnType.containsKey("==");
    } else {
      return false;
    }
  }

  @Fallback
  @TruffleBoundary
  boolean equalsGeneric(Object left, Object right,
      @CachedLibrary(limit = "5") InteropLibrary interop) {
    EnsoContext ctx = EnsoContext.get(interop);
    if (isHostObject(ctx, left) && isHostObject(ctx, right)) {
      try {
        return interop.asBoolean(
            interop.invokeMember(left, "equals", right)
        );
      } catch (UnsupportedMessageException | ArityException | UnknownIdentifierException |
               UnsupportedTypeException e) {
        throw new IllegalStateException(e);
      }
    } else {
      return left == right
          || left.equals(right)
          || interop.isIdentical(left, right, interop);
    }
  }

  private static boolean isHostObject(EnsoContext context, Object object) {
    return context.getEnvironment().isHostObject(object);
  }
}
