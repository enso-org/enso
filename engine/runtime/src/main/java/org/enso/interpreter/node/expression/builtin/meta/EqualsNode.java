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
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnknownKeyException;
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
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
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
import org.enso.polyglot.MethodNames;

@BuiltinMethod(
    type = "Comparable",
    name = "equals_builtin",
    description = """
      Compares self with other object and returns True iff `self` is exactly the same as
      the other object, including all its transitively accessible properties or fields.
      Can handle arbitrary objects, including all foreign objects.
      
      Does not throw exceptions.
      
      Note that this is different than `Meta.is_same_object`, which checks whether two
      references point to the same object on the heap.
      """
)
@GenerateUncached
public abstract class EqualsNode extends Node {

  protected static String EQUALS_MEMBER_NAME = MethodNames.Function.EQUALS;

  public static EqualsNode build() {
    return EqualsNodeGen.create();
  }

  public abstract boolean execute(@AcceptsError Object left, @AcceptsError Object right);

  /**
   * Primitive values
   **/


  @Specialization
  boolean equalsBoolean(boolean self, boolean other) {
    return self == other;
  }

  @Specialization
  boolean equalsBytes(byte self, byte other) {
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

  /**
   * Enso specific types
   **/

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

  @Specialization
  boolean equalsModuleScopes(ModuleScope selfModuleScope, ModuleScope otherModuleScope,
      @Cached EqualsNode equalsNode) {
    return equalsNode.execute(selfModuleScope.getModule(), otherModuleScope.getModule());
  }

  @Specialization
  @TruffleBoundary
  boolean equalsModules(Module selfModule, Module otherModule,
      @Cached EqualsNode equalsNode) {
    return equalsNode.execute(selfModule.getName().toString(), otherModule.getName().toString());
  }

  @Specialization
  boolean equalsFiles(EnsoFile selfFile, EnsoFile otherFile,
      @CachedLibrary(limit = "5") InteropLibrary interop) {
    return equalsStrings(selfFile.getPath(), otherFile.getPath(), interop, interop);
  }

  /**
   * There is no specialization for {@link TypesLibrary#hasType(Object)}, because also
   * primitive values would fall into that specialization and it would be too complicated
   * to make that specialization disjunctive. So we rather specialize directly for
   * {@link Type types}.
   */
  @Specialization(guards = {
      "typesLib.hasType(selfType)",
      "typesLib.hasType(otherType)"
  })
  boolean equalsTypes(Type selfType, Type otherType,
      @Cached EqualsNode equalsNode,
      @CachedLibrary(limit = "5") TypesLibrary typesLib) {
    return equalsNode.execute(
        selfType.getQualifiedName().toString(),
        otherType.getQualifiedName().toString()
    );
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
                             @Cached EqualsNode equalsNode
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

  @Specialization(limit = "3")
  boolean equalsTexts(Text selfText, Text otherText,
      @CachedLibrary("selfText") InteropLibrary selfInterop,
      @CachedLibrary("otherText") InteropLibrary otherInterop) {
    if (selfText.is_normalized() && otherText.is_normalized()) {
      return selfText.toString().compareTo(otherText.toString()) == 0;
    } else {
      return equalsStrings(selfText, otherText, selfInterop, otherInterop);
    }
  }

  /** Interop libraries **/

  @Specialization(guards = {
      "selfInterop.isNull(selfNull) || otherInterop.isNull(otherNull)",
  }, limit = "3")
  boolean equalsNull(
      Object selfNull, Object otherNull,
      @CachedLibrary("selfNull") InteropLibrary selfInterop,
      @CachedLibrary("otherNull") InteropLibrary otherInterop
  ) {
    return selfInterop.isNull(selfNull) && otherInterop.isNull(otherNull);
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
      // We cannot use self.isEqual(other), because that does not include timezone.
      return self.compareTo(other) == 0;
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
      "otherInterop.hasArrayElements(otherArray)",
      "!selfInterop.hasHashEntries(selfArray)",
      "!otherInterop.hasHashEntries(otherArray)",
  }, limit = "3")
  boolean equalsArrays(Object selfArray, Object otherArray,
                       @CachedLibrary("selfArray") InteropLibrary selfInterop,
                       @CachedLibrary("otherArray") InteropLibrary otherInterop,
                       @Cached EqualsNode equalsNode,
      @Cached HasCustomComparatorNode hasCustomComparatorNode,
      @Cached InvokeAnyEqualsNode invokeAnyEqualsNode
  ) {
    try {
      long selfSize = selfInterop.getArraySize(selfArray);
      if (selfSize != otherInterop.getArraySize(otherArray)) {
        return false;
      }
      for (long i = 0; i < selfSize; i++) {
        Object selfElem = selfInterop.readArrayElement(selfArray, i);
        Object otherElem = otherInterop.readArrayElement(otherArray, i);
        boolean elemsAreEqual;
        if (selfElem instanceof Atom selfAtomElem
            && otherElem instanceof Atom otherAtomElem
            && hasCustomComparatorNode.execute(selfAtomElem)) {
          elemsAreEqual = invokeAnyEqualsNode.execute(selfAtomElem, otherAtomElem);
        } else {
          elemsAreEqual = equalsNode.execute(selfElem, otherElem);
        }
        if (!elemsAreEqual) {
          return false;
        }
      }
      return true;
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "selfInterop.hasHashEntries(selfHashMap)",
      "otherInterop.hasHashEntries(otherHashMap)",
      "!selfInterop.hasArrayElements(selfHashMap)",
      "!otherInterop.hasArrayElements(otherHashMap)"
  }, limit = "3")
  boolean equalsHashMaps(Object selfHashMap, Object otherHashMap,
      @CachedLibrary("selfHashMap") InteropLibrary selfInterop,
      @CachedLibrary("otherHashMap") InteropLibrary otherInterop,
      @CachedLibrary(limit = "5") InteropLibrary entriesInterop,
      @Cached EqualsNode equalsNode) {
    try {
      int selfHashSize = (int) selfInterop.getHashSize(selfHashMap);
      int otherHashSize = (int) otherInterop.getHashSize(otherHashMap);
      if (selfHashSize != otherHashSize) {
        return false;
      }
      Object selfEntriesIter = selfInterop.getHashEntriesIterator(selfHashMap);
      while (entriesInterop.hasIteratorNextElement(selfEntriesIter)) {
        Object selfKeyValue = entriesInterop.getIteratorNextElement(selfEntriesIter);
        Object key = entriesInterop.readArrayElement(selfKeyValue, 0);
        Object selfValue = entriesInterop.readArrayElement(selfKeyValue, 1);
        if (otherInterop.isHashEntryExisting(otherHashMap, key)
            && otherInterop.isHashEntryReadable(otherHashMap, key)) {
          Object otherValue = otherInterop.readHashValue(otherHashMap, key);
          if (!equalsNode.execute(selfValue, otherValue)) {
            return false;
          }
        } else {
          return false;
        }
      }
      return true;
    } catch (UnsupportedMessageException | StopIterationException | UnknownKeyException |
             InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "!isAtom(selfObject)",
      "!isAtom(otherObject)",
      "!isHostObject(selfObject)",
      "!isHostObject(otherObject)",
      "interop.hasMembers(selfObject)",
      "interop.hasMembers(otherObject)",
      "!interop.isDate(selfObject)",
      "!interop.isDate(otherObject)",
      "!interop.isTime(selfObject)",
      "!interop.isTime(otherObject)",
      // Objects with types are handled in `equalsTypes` specialization, so we have to
      // negate the guards of that specialization here - to make the specializations
      // disjunctive.
      "!typesLib.hasType(selfObject)",
      "!typesLib.hasType(otherObject)",
  })
  boolean equalsInteropObjectWithMembers(Object selfObject, Object otherObject,
      @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "5") TypesLibrary typesLib,
      @Cached EqualsNode equalsNode) {
    try {
      Object selfMembers = interop.getMembers(selfObject);
      Object otherMembers = interop.getMembers(otherObject);
      assert interop.getArraySize(selfMembers) < Integer.MAX_VALUE : "Long array sizes not supported";
      int membersSize = (int) interop.getArraySize(selfMembers);
      if (interop.getArraySize(otherMembers) != membersSize) {
        return false;
      }

      // Check member names
      String[] memberNames = new String[membersSize];
      for (int i = 0; i < membersSize; i++) {
        String selfMemberName = interop.asString(interop.readArrayElement(selfMembers, i));
        String otherMemberName = interop.asString(interop.readArrayElement(otherMembers, i));
        if (!equalsNode.execute(selfMemberName, otherMemberName)) {
          return false;
        }
        memberNames[i] = selfMemberName;
      }

      // Check member values
      for (int i = 0; i < membersSize; i++) {
        if (interop.isMemberReadable(selfObject, memberNames[i]) &&
            interop.isMemberReadable(otherObject, memberNames[i])) {
          Object selfMember = interop.readMember(selfObject, memberNames[i]);
          Object otherMember = interop.readMember(otherObject, memberNames[i]);
          if (!equalsNode.execute(selfMember, otherMember)) {
            return false;
          }
        }
      }
      return true;
    } catch (UnsupportedMessageException | InvalidArrayIndexException | UnknownIdentifierException e) {
      throw new IllegalStateException(
          String.format("One of the interop objects has probably wrongly specified interop API "
              + "for members. selfObject = %s ; otherObject = %s", selfObject, otherObject),
          e
      );
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

  @Specialization
  boolean equalsAtoms(
      Atom self,
      Atom other,
      @Cached LoopConditionProfile loopProfile,
      @Cached(value = "createEqualsNodes(equalsNodeCountForFields)", allowUncached = true) EqualsNode[] fieldEqualsNodes,
      @Cached ConditionProfile enoughEqualNodesForFieldsProfile,
      @Cached ConditionProfile constructorsNotEqualProfile,
      @CachedLibrary(limit = "3") StructsLibrary selfStructs,
      @CachedLibrary(limit = "3") StructsLibrary otherStructs,
      @Cached HasCustomComparatorNode hasCustomComparatorNode,
      @Cached InvokeAnyEqualsNode invokeAnyEqualsNode
  ) {
    if (constructorsNotEqualProfile.profile(
        self.getConstructor() != other.getConstructor()
    )) {
      return false;
    }
    var selfFields = selfStructs.getFields(self);
    var otherFields = otherStructs.getFields(other);
    assert selfFields.length == otherFields.length;

    int fieldsSize = selfFields.length;
    if (enoughEqualNodesForFieldsProfile.profile(fieldsSize <= equalsNodeCountForFields)) {
      loopProfile.profileCounted(fieldsSize);
      for (int i = 0; loopProfile.inject(i < fieldsSize); i++) {
        boolean fieldsAreEqual;
        // We don't check whether `other` has the same type of comparator, that is checked in
        // `Any.==` that we invoke here anyway.
        if (selfFields[i] instanceof Atom selfAtomField
            && otherFields[i] instanceof Atom otherAtomField
            && hasCustomComparatorNode.execute(selfAtomField)) {
          // If selfFields[i] has a custom comparator, we delegate to `Any.==` that deals with
          // custom comparators. EqualsNode cannot deal with custom comparators.
          fieldsAreEqual = invokeAnyEqualsNode.execute(selfAtomField, otherAtomField);
        } else {
          fieldsAreEqual = fieldEqualsNodes[i].execute(selfFields[i], otherFields[i]);
        }
        if (!fieldsAreEqual) {
          return false;
        }
      }
    } else {
      return equalsAtomsFieldsUncached(selfFields, otherFields);
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
          && HasCustomComparatorNode.getUncached().execute(selfFieldAtom)) {
        areFieldsSame = InvokeAnyEqualsNode.getUncached().execute(selfFieldAtom, otherFieldAtom);
      } else {
        areFieldsSame = EqualsNodeGen.getUncached().execute(selfFields[i], otherFields[i]);
      }
      if (!areFieldsSame) {
        return false;
      }
    }
    return true;
  }

  @Specialization(guards = {
      "isHostObject(selfHostObject)",
      "isHostObject(otherHostObject)"
  })
  boolean equalsHostObjects(
      Object selfHostObject, Object otherHostObject,
      @CachedLibrary(limit = "5") InteropLibrary interop
  ) {
    try {
      return interop.asBoolean(
          interop.invokeMember(selfHostObject, "equals", otherHostObject)
      );
    } catch (UnsupportedMessageException | ArityException | UnknownIdentifierException |
             UnsupportedTypeException e) {
      throw new IllegalStateException(e);
    }
  }

  // HostFunction is identified by a qualified name, it is not a lambda.
  // It has well-defined equality based on the qualified name.
  @Specialization(guards = {
      "isHostFunction(selfHostFunc)",
      "isHostFunction(otherHostFunc)"
  })
  boolean equalsHostFunctions(Object selfHostFunc, Object otherHostFunc,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Cached EqualsNode equalsNode) {
    Object selfFuncStrRepr = interop.toDisplayString(selfHostFunc);
    Object otherFuncStrRepr = interop.toDisplayString(otherHostFunc);
    return equalsNode.execute(selfFuncStrRepr, otherFuncStrRepr);
  }

  @Fallback
  @TruffleBoundary
  boolean equalsGeneric(Object left, Object right,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @CachedLibrary(limit = "5") TypesLibrary typesLib) {
      return left == right
          || interop.isIdentical(left, right, interop)
          || left.equals(right)
          || (isNullOrNothing(left, typesLib, interop) && isNullOrNothing(right, typesLib, interop));
  }

  private boolean isNullOrNothing(Object object, TypesLibrary typesLib, InteropLibrary interop) {
    if (typesLib.hasType(object)) {
      return typesLib.getType(object) == EnsoContext.get(this).getNothing();
    } else if (interop.isNull(object)) {
      return true;
    } else {
      return object == null;
    }
  }

  static boolean isAtom(Object object) {
    return object instanceof Atom;
  }

  @TruffleBoundary
  boolean isHostObject(Object object) {
    return EnsoContext.get(this).getEnvironment().isHostObject(object);
  }

  @TruffleBoundary
  boolean isHostFunction(Object object) {
    return EnsoContext.get(this).getEnvironment().isHostFunction(object);
  }

  /**
   * Helper node for invoking `Any.==` method.
   */
  @GenerateUncached
  static abstract class InvokeAnyEqualsNode extends Node {
    static InvokeAnyEqualsNode getUncached() {
      return EqualsNodeGen.InvokeAnyEqualsNodeGen.getUncached();
    }

    abstract boolean execute(Atom selfAtom, Atom otherAtom);

    @Specialization
    boolean invokeEqualsCachedAtomCtor(Atom selfAtom, Atom thatAtom,
        @Cached(value = "getAnyEqualsMethod()", allowUncached = true) Function anyEqualsFunc,
        @Cached(value = "buildInvokeFuncNodeForAnyEquals()", allowUncached = true) InvokeFunctionNode invokeAnyEqualsNode,
        @CachedLibrary(limit = "3") InteropLibrary interop) {
      // TODO: Shouldn't Comparable type be the very first argument? (synthetic self)?
      Object ret = invokeAnyEqualsNode.execute(
          anyEqualsFunc,
          null,
          State.create(EnsoContext.get(this)),
          new Object[]{selfAtom, thatAtom}
      );
      try {
        return interop.asBoolean(ret);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException("Return value from Any== should be Boolean", e);
      }

    }

    @TruffleBoundary
    Function getAnyEqualsMethod() {
      var anyType = EnsoContext.get(this).getBuiltins().any();
      Function anyEqualsFunc =
          anyType.getDefinitionScope().getMethods().get(anyType).get("==");
      assert anyEqualsFunc != null : "Any.== method must exist";
      return anyEqualsFunc;
    }

    InvokeFunctionNode buildInvokeFuncNodeForAnyEquals() {
      return InvokeFunctionNode.build(
          new CallArgumentInfo[]{new CallArgumentInfo("self"), new CallArgumentInfo("that")},
          DefaultsExecutionMode.EXECUTE,
          ArgumentsExecutionMode.EXECUTE
      );
    }
  }
}
