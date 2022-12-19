package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.dsl.SpecializationStatistics;
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
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.ExecuteCallNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNodeGen.InvokeEqualsNodeGen;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.state.State;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.MethodNames;

@BuiltinMethod(
    type = "Any",
    name = "equals_builtin",
    description = "Implementation of Any.=="
)
@GenerateUncached
@SpecializationStatistics.AlwaysEnabled
public abstract class EqualsNode extends Node {

  protected static String EQUALS_MEMBER_NAME = MethodNames.Function.EQUALS;

  public static EqualsNode build() {
    return EqualsNodeGen.create();
  }
  public abstract boolean execute(@AcceptsError Object self, @AcceptsError Object right);

  /** Primitive values **/

  @Specialization
  boolean equalsLong(long self, long other) {
    return self == other;
  }

  @Specialization
  boolean equalsBoolean(boolean self, boolean other) {
    return self == other;
  }

  @Specialization
  boolean equalsDouble(double self, double other) {
    return self == other;
  }

  @Specialization
  @TruffleBoundary
  boolean equalsBigInt(EnsoBigInteger self, EnsoBigInteger otherBigInt) {
    return self.equals(otherBigInt);
  }

  /** Enso specific types **/

  @Specialization
  boolean equalsUnresolvedSymbols(UnresolvedSymbol self, UnresolvedSymbol otherSymbol,
      @Cached EqualsNode equalsNode) {
    return self.getName().equals(otherSymbol.getName())
        && equalsNode.execute(self.getScope(), otherSymbol.getScope());
  }

  @Specialization
  boolean equalsUnresolvedConversion(UnresolvedConversion selfConversion, UnresolvedConversion otherConversion,
      @Cached EqualsNode equalsNode) {
    return equalsNode.execute(selfConversion.getScope(), otherConversion.getScope());
  }

  /** Interop libraries **/

  @Specialization(guards = {
      "selfInterop.isMetaObject(selfMeta)",
      "otherInterop.isMetaObject(otherMeta)",
  }, limit = "3")
  boolean equalsMetaObjects(Object selfMeta, Object otherMeta,
      @CachedLibrary("selfMeta") InteropLibrary selfInterop,
      @CachedLibrary("otherMeta") InteropLibrary otherInterop,
      @Cached EqualsNode equalsNode) {
    try {
      Object selfMetaName = selfInterop.getMetaQualifiedName(selfMeta);
      Object otherMetaName = otherInterop.getMetaQualifiedName(otherMeta);
      return equalsNode.execute(selfMetaName, otherMetaName);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "selfInterop.isDate(selfDateTime)",
      "selfInterop.isTime(selfDateTime)",
      "otherInterop.isDate(otherDateTime)",
      "otherInterop.isTime(otherDateTime)"
  }, limit = "3")
  boolean equalsDateTimes(Object selfDateTime, Object otherDateTime,
      @CachedLibrary("selfDateTime") InteropLibrary selfInterop,
      @CachedLibrary("otherDateTime") InteropLibrary otherInterop) {
    LocalDateTime selfJavaDateTime;
    LocalDateTime otherJavaDateTime;
    try {
      selfJavaDateTime = LocalDateTime.of(
          selfInterop.asDate(selfDateTime),
          selfInterop.asTime(selfDateTime)
      );
      otherJavaDateTime = LocalDateTime.of(
          otherInterop.asDate(otherDateTime),
          otherInterop.asTime(otherDateTime)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    return selfJavaDateTime.isEqual(otherJavaDateTime);
  }

  @Specialization(guards = {
      "selfInterop.isDate(selfDate)",
      "!selfInterop.isTime(selfDate)",
      "otherInterop.isDate(otherDate)",
      "!otherInterop.isTime(otherDate)"
  }, limit = "3")
  boolean equalsDates(Object selfDate, Object otherDate,
      @CachedLibrary("selfDate") InteropLibrary selfInterop,
      @CachedLibrary("otherDate") InteropLibrary otherInterop) {
    LocalDate otherJavaDate;
    LocalDate selfJavaDate;
    try {
      otherJavaDate = otherInterop.asDate(otherDate);
      selfJavaDate = selfInterop.asDate(selfDate);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    return selfJavaDate.isEqual(otherJavaDate);
  }

  @Specialization(guards = {
      "!selfInterop.isDate(selfTime)",
      "selfInterop.isTime(selfTime)",
      "!otherInterop.isDate(otherTime)",
      "otherInterop.isTime(otherTime)"
  }, limit = "3")
  boolean equalsTimes(Object selfTime, Object otherTime,
      @CachedLibrary("selfTime") InteropLibrary selfInterop,
      @CachedLibrary("otherTime") InteropLibrary otherInterop) {
    LocalTime selfJavaTime;
    LocalTime otherJavaTime;
    try {
      selfJavaTime = selfInterop.asTime(selfTime);
      otherJavaTime = otherInterop.asTime(otherTime);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    return selfJavaTime.equals(otherJavaTime);
  }

  @Specialization(guards = {
      "selfInterop.isDuration(selfDuration)",
      "otherInterop.isDuration(otherDuration)"
  }, limit = "3")
  boolean equalsDuration(Object selfDuration, Object otherDuration,
      @CachedLibrary("selfDuration") InteropLibrary selfInterop,
      @CachedLibrary("otherDuration") InteropLibrary otherInterop) {
    Duration selfJavaDuration;
    Duration otherJavaDuration;
    try {
      selfJavaDuration = selfInterop.asDuration(selfDuration);
      otherJavaDuration = otherInterop.asDuration(otherDuration);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    return selfJavaDuration.equals(otherJavaDuration);
  }

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
    return selfJavaString.equals(otherJavaString);
  }

  @Specialization(guards = {
      "selfInterop.isExecutable(selfFunction)",
      "otherInterop.isExecutable(otherFunction)",
      "selfInterop.isMemberInvocable(selfFunction, EQUALS_MEMBER_NAME)",
      "otherInterop.isMemberInvocable(otherFunction, EQUALS_MEMBER_NAME)",
  }, limit = "3")
  boolean equalsFunctions(Object selfFunction, Object otherFunction,
      @CachedLibrary("selfFunction") InteropLibrary selfInterop,
      @CachedLibrary("otherFunction") InteropLibrary otherInterop,
      @CachedLibrary(limit = "3") InteropLibrary retValueInterop) {
    Object ret;
    try {
      ret = selfInterop.invokeMember(selfFunction, MethodNames.Function.EQUALS, otherFunction);
      return retValueInterop.asBoolean(ret);
    } catch (UnsupportedMessageException | ArityException | UnknownIdentifierException |
             UnsupportedTypeException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "selfInterop.hasArrayElements(selfArray)",
      "otherInterop.hasArrayElements(otherArray)"
  }, limit = "3")
  boolean equalsArrays(Object selfArray, Object otherArray,
      @CachedLibrary("selfArray") InteropLibrary selfInterop,
      @CachedLibrary("otherArray") InteropLibrary otherInterop,
      @Cached EqualsNode equalsNode
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

  /**
   * Behind truffle boundary, because comparing exceptions is not expected to be on fast path.
   * @return True if the givne exceptions are the same
   */
  @TruffleBoundary
  @Specialization(guards = {
      "selfInterop.isException(selfException)",
      "otherInterop.isException(otherException)",
  }, limit = "3")
  boolean equalsExceptions(Object selfException, Object otherException,
      @CachedLibrary("selfException") InteropLibrary selfInterop,
      @CachedLibrary("otherException") InteropLibrary otherInterop,
      @CachedLibrary(limit = "5") InteropLibrary interop) {
    try {
      boolean sameExceptionTypes =
          selfInterop.getExceptionType(selfException) == otherInterop.getExceptionType(otherException);

      String selfMessage = null;
      if (selfInterop.hasExceptionMessage(selfException)) {
        selfMessage = interop.asString(
            selfInterop.getExceptionMessage(selfException)
        );
      }
      String otherMessage = null;
      if (otherInterop.hasExceptionMessage(otherException)) {
        otherMessage = interop.asString(
            otherInterop.getExceptionMessage(otherException)
        );
      }
      boolean sameMessages = Objects.equals(selfMessage, otherMessage);

      Object selfCause =
          selfInterop.hasExceptionCause(selfException) ? selfInterop.getExceptionCause(selfException) : null;
      Object otherCause =
          otherInterop.hasExceptionCause(otherException) ? otherInterop.getExceptionCause(otherException) : null;
      boolean sameCauses = Objects.equals(selfCause, otherCause);

      return sameExceptionTypes && sameMessages && sameCauses;
    } catch (UnsupportedMessageException e) {
      throw new RuntimeException(e);
    }
  }

  /** Equals for Atoms and AtomConstructors */

  @Specialization
  boolean equalsAtomConstructors(AtomConstructor selfConstructor, AtomConstructor otherConstructor) {
    return selfConstructor == otherConstructor;
  }

  /**
   * How many {@link EqualsNode} should be created for fields in specialization for atoms.
   */
  static final int equalsNodeCountForFields = 10;

  static EqualsNode[] createEqualsNodes(int size) {
    EqualsNode[] nodes = new EqualsNode[size];
    Arrays.fill(nodes, EqualsNode.build());
    return nodes;
  }

  static InvokeEqualsNode[] createInvokeEqualsNodes(int size) {
    InvokeEqualsNode[] nodes = new InvokeEqualsNode[size];
    Arrays.fill(nodes, InvokeEqualsNode.build());
    return nodes;
  }

  @Specialization
  boolean equalsAtoms(Atom self, Atom otherAtom,
      @Cached LoopConditionProfile loopProfile,
      @Cached(value = "createEqualsNodes(equalsNodeCountForFields)", allowUncached = true) EqualsNode[] fieldEqualsNodes,
      @Cached(value = "createInvokeEqualsNodes(equalsNodeCountForFields)", allowUncached = true) InvokeEqualsNode[] fieldInvokeEqualsNodes,
      @Cached ConditionProfile enoughEqualNodesForFieldsProfile,
      @Cached ConditionProfile fieldsSizeNotEqualProfile,
      @Cached ConditionProfile constructorsNotEqualProfile) {
    if (fieldsSizeNotEqualProfile.profile(
        self.getFields().length != otherAtom.getFields().length
    )) {
      return false;
    }
    if (constructorsNotEqualProfile.profile(
        self.getConstructor() != otherAtom.getConstructor()
    )) {
      return false;
    }

    assert self.getFields().length == otherAtom.getFields().length;
    int fieldsSize = self.getFields().length;
    if (enoughEqualNodesForFieldsProfile.profile(fieldsSize <= equalsNodeCountForFields)) {
      loopProfile.profileCounted(fieldsSize);
      for (int i = 0; loopProfile.inject(i < fieldsSize); i++) {
        boolean areFieldsSame;
        // Note that otherFieldAtom does not have to override `==` method, it is sufficient if
        // selfFieldAtom overrides `==` method.
        Object selfField = self.getFields()[i];
        Object otherField = otherAtom.getFields()[i];
        if (selfField instanceof Atom selfFieldAtom
            && otherField instanceof Atom otherFieldAtom
            && atomOverridesEquals(selfFieldAtom)
        ) {
          areFieldsSame =
              fieldInvokeEqualsNodes[i].execute(selfFieldAtom, otherFieldAtom);
        } else {
          areFieldsSame = fieldEqualsNodes[i].execute(
              selfField,
              otherField
          );
        }
        if (!areFieldsSame) {
          return false;
        }
      }
    } else {
      // If there are more fields than equalsNodeCountForFields, just bailout and use
      // uncached variant of EqualsNode and InvokeEqualsNode
      CompilerDirectives.transferToInterpreterAndInvalidate();
      for (int i = 0; i < fieldsSize; i++) {
        Object selfField = self.getFields()[i];
        Object otherField = otherAtom.getFields()[i];
        boolean areFieldsSame;
        if (selfField instanceof Atom selfFieldAtom
            && otherField instanceof Atom otherFieldAtom
            && atomOverridesEquals(selfFieldAtom)) {
          areFieldsSame = InvokeEqualsNode.getUncached().execute(selfFieldAtom, otherFieldAtom);
        } else {
          areFieldsSame = EqualsNodeGen.getUncached().execute(selfField, otherField);
        }
        if (!areFieldsSame) {
          return false;
        }
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
