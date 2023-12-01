package org.enso.interpreter.node.expression.builtin.meta;

import com.google.common.base.Objects;
import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.profiles.LoopConditionProfile;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Arrays;

import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.node.expression.builtin.ordering.CustomComparatorNode;
import org.enso.interpreter.node.expression.builtin.ordering.CustomComparatorNodeGen;
import org.enso.interpreter.node.expression.builtin.ordering.HashCallbackNode;
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
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.State;
import org.enso.polyglot.common_utils.Core_Text_Utils;

import com.oracle.truffle.api.dsl.Fallback;

/**
 * Implements {@code hash_code} functionality.
 *
 * <p>Make sure that the hashing contract is retained after any modification.
 *
 * <h3>Hashing contract:</h3>
 *
 * <ul>
 *   <li>Whenever two objects are equal ({@code EqualsNode} returns {@code true}), their hashcode
 *       should equal. More formally: {@code For all objects o1, o2: if o1 == o2 then hash(o1) ==
 *       hash(o2)}
 *   <li>Whenever two hash codes are different, their associated objects are different: {@code For all objects
 *       o1, o2: if hash(o1) != hash(o2) then o1 != o2.
 * </ul>
 */
@GenerateUncached
@BuiltinMethod(
    type = "Comparable",
    name = "hash_builtin",
    description = """
        Returns hash code of this atom. Use only for overriding default Comparator.
        """
)
public abstract class HashCodeNode extends Node {

  public static HashCodeNode build() {
    return HashCodeNodeGen.create();
  }

  public abstract long execute(@AcceptsError Object object);

  /** Specializations for primitive values * */
  @Specialization
  long hashCodeForShort(short s) {
    return s;
  }

  @Specialization
  long hashCodeForByte(byte b) {
    return b;
  }

  @Specialization
  long hashCodeForLong(long l) {
    // By casting long to double, we lose some precision on purpose
    return hashCodeForDouble((double) l);
  }

  @Specialization
  long hashCodeForInt(int i) {
    return hashCodeForLong(i);
  }

  @Specialization
  long hashCodeForFloat(float f) {
    return Float.hashCode(f);
  }

  @Specialization
  long hashCodeForDouble(double d) {
    if (Double.isNaN(d)) {
      // NaN is Incomparable, just return a "random" constant
      return 456879;
    } else if (d % 1.0 != 0 || BigIntegerOps.fitsInLong(d)) {
      return Double.hashCode(d);
    } else {
      return bigDoubleHash(d);
    }
  }

  @TruffleBoundary
  private static long bigDoubleHash(double d) {
    try {
      return BigDecimal.valueOf(d).toBigIntegerExact().hashCode();
    } catch (ArithmeticException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization
  @TruffleBoundary
  long hashCodeForBigInteger(EnsoBigInteger bigInteger) {
    return bigInteger.getValue().hashCode();
  }

  @Specialization
  long hashCodeForAtomConstructor(AtomConstructor atomConstructor) {
    // AtomConstructors are singletons, we take system hash code explicitly.
    return System.identityHashCode(atomConstructor);
  }

  @Specialization
  @TruffleBoundary
  long hashCodeForUnresolvedSymbol(UnresolvedSymbol unresolvedSymbol,
      @Shared("hashCodeNode") @Cached HashCodeNode hashCodeNode) {
    long nameHash = hashCodeNode.execute(unresolvedSymbol.getName());
    long scopeHash = hashCodeNode.execute(unresolvedSymbol.getScope());
    return Objects.hashCode(nameHash, scopeHash);
  }

  @Specialization
  long hashCodeForUnresolvedConversion(UnresolvedConversion unresolvedConversion,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    return hashCodeForModuleScope(unresolvedConversion.getScope(), interop);
  }

  @Specialization
  long hashCodeForModuleScope(ModuleScope moduleScope,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    return hashCodeForModule(moduleScope.getModule(), interop);
  }

  @Specialization
  @TruffleBoundary
  long hashCodeForModule(Module module,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    return hashCodeForString(module.toString(), interop);
  }

  @Specialization
  long hashCodeForFile(EnsoFile file,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    return hashCodeForString(file.getPath(), interop);
  }

  /**
   * There is no specialization for {@link TypesLibrary#hasType(Object)}, because also
   * primitive values would fall into that specialization and it would be too complicated
   * to make that specialization disjunctive. So we rather specialize directly for
   * {@link Type}.
   */
  @Specialization
  long hashCodeForType(Type type,
      @Shared("hashCodeNode") @Cached HashCodeNode hashCodeNode) {
    if (EnsoContext.get(this).getNothing() == type) {
      // Nothing should be equal to `null`
      return 0;
    } else {
      return hashCodeNode.execute(type.getQualifiedName().toString());
    }
  }

  @NeverDefault
  static HashCodeNode[] createHashCodeNodes(int size) {
    HashCodeNode[] nodes = new HashCodeNode[size];
    Arrays.fill(nodes, HashCodeNode.build());
    return nodes;
  }

  @Specialization(guards = {
      "atomCtorCached == atom.getConstructor()",
      "customComparatorNode.execute(atom) == null",
  }, limit = "5")
  @ExplodeLoop
  long hashCodeForAtomWithDefaultComparator(
      Atom atom,
      @Cached("atom.getConstructor()") AtomConstructor atomCtorCached,
      @Cached("atomCtorCached.getFields().length") int fieldsLenCached,
      @Cached(value = "createHashCodeNodes(fieldsLenCached)", allowUncached = true)
          HashCodeNode[] fieldHashCodeNodes,
      @Cached ConditionProfile isHashCodeCached,
      @CachedLibrary(limit = "10") StructsLibrary structs,
      @Shared("customComparatorNode") @Cached CustomComparatorNode customComparatorNode,
      @Shared("hashCallbackNode") @Cached HashCallbackNode hashCallbackNode) {
    if (isHashCodeCached.profile(atom.getHashCode() != null)) {
      return atom.getHashCode();
    }

    Object[] fields = structs.getFields(atom);
    int fieldsCount = fields.length;

    CompilerAsserts.partialEvaluationConstant(fieldsLenCached);
    // hashes stores hash codes for all fields, and for constructor.
    int[] hashes = new int[fieldsCount + 1];
    for (int i = 0; i < fieldsLenCached; i++) {
      if (fields[i] instanceof Atom atomField && customComparatorNode.execute(atomField) != null) {
        hashes[i] = (int) hashCallbackNode.execute(atomField);
      } else {
        hashes[i] = (int) fieldHashCodeNodes[i].execute(fields[i]);
      }
    }

    int ctorHashCode = (int) hashCodeForAtomConstructor(atom.getConstructor());
    hashes[hashes.length - 1] = ctorHashCode;

    int atomHashCode = Arrays.hashCode(hashes);
    atom.setHashCode(atomHashCode);
    return atomHashCode;
  }

  @Specialization(
      guards = {
        "atomCtorCached == atom.getConstructor()",
        "cachedComparator != null"
      },
      limit = "5"
  )
  long hashCodeForAtomWithCustomComparator(
      Atom atom,
      @Cached("atom.getConstructor()") AtomConstructor atomCtorCached,
      @Shared("customComparatorNode") @Cached CustomComparatorNode customComparatorNode,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Cached(value = "customComparatorNode.execute(atom)") Type cachedComparator,
      @Cached(value = "findHashMethod(cachedComparator)", allowUncached = true)
        Function compareMethod,
      @Cached(value = "createInvokeNode(compareMethod)") InvokeFunctionNode invokeFunctionNode
  ) {
    var ctx = EnsoContext.get(this);
    var args = new Object[] { cachedComparator, atom};
    var result = invokeFunctionNode.execute(compareMethod, null, State.create(ctx), args);
    if (!interop.isNumber(result)) {
      throw new PanicException("Custom comparator must return a number", this);
    } else {
      try {
        return interop.asLong(result);
      } catch (UnsupportedMessageException e) {
        throw new IllegalStateException(e);
      }
    }
  }

  @TruffleBoundary
  static Function findHashMethod(Type comparator) {
    var fn = comparator.getDefinitionScope().getMethodForType(comparator, "hash");
    if (fn == null) {
      throw new AssertionError("No hash method for type " + comparator);
    }
    return fn;
  }

  @NeverDefault
  static InvokeFunctionNode createInvokeNode(Function compareFn) {
    CallArgumentInfo[] argsInfo = new CallArgumentInfo[compareFn.getSchema().getArgumentsCount()];
    for (int i = 0; i < argsInfo.length; i++) {
      var argDef = compareFn.getSchema().getArgumentInfos()[i];
      argsInfo[i] = new CallArgumentInfo(argDef.getName());
    }
    return InvokeFunctionNode.build(
        argsInfo, DefaultsExecutionMode.EXECUTE, ArgumentsExecutionMode.EXECUTE);
  }

  @TruffleBoundary
  @Specialization(replaces = {"hashCodeForAtomWithDefaultComparator", "hashCodeForAtomWithCustomComparator"})
  long hashCodeForAtomUncached(Atom atom) {
    if (atom.getHashCode() != null) {
      return atom.getHashCode();
    }

    Type customComparator = CustomComparatorNode.getUncached().execute(atom);
    if (customComparator != null) {
      Function compareMethod = findHashMethod(customComparator);
      return hashCodeForAtomWithCustomComparator(
          atom,
          atom.getConstructor(),
          CustomComparatorNodeGen.getUncached(),
          InteropLibrary.getFactory().getUncached(),
          customComparator,
          compareMethod,
          createInvokeNode(compareMethod)
      );
    }

    Object[] fields = StructsLibrary.getUncached().getFields(atom);
    int[] hashes = new int[fields.length + 1];
    for (int i = 0; i < fields.length; i++) {
      if (fields[i] instanceof Atom atomField
          && CustomComparatorNode.getUncached().execute(atomField) != null) {
        hashes[i] = (int) HashCallbackNode.getUncached().execute(atomField);
      } else {
        hashes[i] = (int) HashCodeNodeGen.getUncached().execute(fields[i]);
      }
    }

    int ctorHashCode = (int) hashCodeForAtomConstructor(atom.getConstructor());
    hashes[hashes.length - 1] = ctorHashCode;

    int atomHashCode = Arrays.hashCode(hashes);
    atom.setHashCode(atomHashCode);
    return atomHashCode;
  }

  @Specialization(
      guards = {"warnLib.hasWarnings(selfWithWarning)"},
      limit = "3")
  long hashCodeForWarning(
      Object selfWithWarning,
      @CachedLibrary("selfWithWarning") WarningsLibrary warnLib,
      @Shared("hashCodeNode") @Cached HashCodeNode hashCodeNode) {
    try {
      return hashCodeNode.execute(warnLib.removeWarnings(selfWithWarning));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  /** Specializations for interop values * */
  @Specialization(
      guards = {"interop.isBoolean(selfBool)"},
      limit = "3")
  long hashCodeForBooleanInterop(
      Object selfBool, @CachedLibrary("selfBool") InteropLibrary interop) {
    try {
      return Boolean.hashCode(interop.asBoolean(selfBool));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @TruffleBoundary
  @Specialization(
      guards = {
        "!interop.isDate(selfTimeZone)",
        "!interop.isTime(selfTimeZone)",
        "interop.isTimeZone(selfTimeZone)",
      },
      limit = "3")
  long hashCodeForTimeZoneInterop(
      Object selfTimeZone, @CachedLibrary("selfTimeZone") InteropLibrary interop) {
    try {
      return interop.asTimeZone(selfTimeZone).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @TruffleBoundary
  @Specialization(
      guards = {
        "interop.isDate(selfZonedDateTime)",
        "interop.isTime(selfZonedDateTime)",
        "interop.isTimeZone(selfZonedDateTime)",
      },
      limit = "3")
  long hashCodeForZonedDateTimeInterop(
      Object selfZonedDateTime, @CachedLibrary("selfZonedDateTime") InteropLibrary interop) {
    try {
      return ZonedDateTime.of(
              interop.asDate(selfZonedDateTime),
              interop.asTime(selfZonedDateTime),
              interop.asTimeZone(selfZonedDateTime))
          .hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(
      guards = {
        "interop.isDate(selfDateTime)",
        "interop.isTime(selfDateTime)",
        "!interop.isTimeZone(selfDateTime)",
      },
      limit = "3")
  long hashCodeForDateTimeInterop(
      Object selfDateTime, @CachedLibrary("selfDateTime") InteropLibrary interop) {
    try {
      return LocalDateTime.of(interop.asDate(selfDateTime), interop.asTime(selfDateTime))
          .hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(
      guards = {
        "!interop.isDate(selfTime)",
        "interop.isTime(selfTime)",
        "!interop.isTimeZone(selfTime)",
      },
      limit = "3")
  long hashCodeForTimeInterop(Object selfTime, @CachedLibrary("selfTime") InteropLibrary interop) {
    try {
      return interop.asTime(selfTime).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(
      guards = {
        "interop.isDate(selfDate)",
        "!interop.isTime(selfDate)",
        "!interop.isTimeZone(selfDate)",
      },
      limit = "3")
  long hashCodeForDateInterop(Object selfDate, @CachedLibrary("selfDate") InteropLibrary interop) {
    try {
      return interop.asDate(selfDate).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(
      guards = {
        "interop.isDuration(selfDuration)",
      },
      limit = "3")
  long hashCodeForDurationInterop(
      Object selfDuration, @CachedLibrary("selfDuration") InteropLibrary interop) {
    try {
      return interop.asDuration(selfDuration).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization
  long hashCodeForText(
      Text text,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    if (text.is_normalized()) {
      return text.toString().hashCode();
    } else {
      return hashCodeForString(text, interop);
    }
  }

  @TruffleBoundary
  @Specialization(
      guards = {"interop.isString(selfStr)"},
      limit = "3")
  long hashCodeForString(
      Object selfStr,
      @CachedLibrary("selfStr") InteropLibrary interop) {
    String str;
    try {
      str = interop.asString(selfStr);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    return Core_Text_Utils.unicodeNormalizedHashCode(str);
  }

  @Specialization(
      guards = {
          "interop.hasArrayElements(selfArray)",
          "!interop.hasHashEntries(selfArray)"
      },
      limit = "3")
  long hashCodeForArray(
      Object selfArray,
      @CachedLibrary("selfArray") InteropLibrary interop,
      @Shared("hashCodeNode") @Cached HashCodeNode hashCodeNode,
      @Cached LoopConditionProfile loopProfile,
      @Shared("hashCallbackNode") @Cached HashCallbackNode hashCallbackNode,
      @Shared("customComparatorNode") @Cached CustomComparatorNode customComparatorNode) {
    try {
      long arraySize = interop.getArraySize(selfArray);
      loopProfile.profileCounted(arraySize);
      int[] elemHashCodes = new int[(int) arraySize];
      for (int i = 0; loopProfile.inject(i < arraySize); i++) {
        if (interop.isArrayElementReadable(selfArray, i)) {
          Object elem = interop.readArrayElement(selfArray, i);
          if (elem instanceof Atom atomElem && customComparatorNode.execute(atomElem) != null) {
            elemHashCodes[i] = (int) hashCallbackNode.execute(atomElem);
          } else {
            elemHashCodes[i] = (int) hashCodeNode.execute(elem);
          }
        }
      }
      return Arrays.hashCode(elemHashCodes);
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * Two maps are considered equal, if they have the same entries. Note that we do not care about
   * ordering.
   */
  @Specialization(guards = {
      "interop.hasHashEntries(selfMap)",
      "!interop.hasArrayElements(selfMap)",
  })
  long hashCodeForMap(
      Object selfMap,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("hashCodeNode") @Cached HashCodeNode hashCodeNode) {
    int mapSize;
    long keysHashCode = 0;
    long valuesHashCode = 0;
    try {
      mapSize = (int) interop.getHashSize(selfMap);
      Object entriesIterator = interop.getHashEntriesIterator(selfMap);
      while (interop.hasIteratorNextElement(entriesIterator)) {
        Object entry = interop.getIteratorNextElement(entriesIterator);
        Object key = interop.readArrayElement(entry, 0);
        Object value = interop.readArrayElement(entry, 1);
        // We don't care about the order of keys and values, so we just sum all their hash codes.
        keysHashCode += hashCodeNode.execute(key);
        valuesHashCode += hashCodeNode.execute(value);
      }
    } catch (UnsupportedMessageException | StopIterationException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
    return Arrays.hashCode(new long[] {keysHashCode, valuesHashCode, mapSize});
  }

  @Specialization(guards = {
      "!isAtom(objectWithMembers)",
      "!isJavaObject(objectWithMembers)",
      "interop.hasMembers(objectWithMembers)",
      "!interop.hasArrayElements(objectWithMembers)",
      "!interop.isTime(objectWithMembers)",
      "!interop.isDate(objectWithMembers)",
      "!interop.isTimeZone(objectWithMembers)",
      "!typesLib.hasType(objectWithMembers)",
  })
  long hashCodeForInteropObjectWithMembers(Object objectWithMembers,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @CachedLibrary(limit = "5") TypesLibrary typesLib,
      @Shared("hashCodeNode") @Cached HashCodeNode hashCodeNode) {
    try {
      Object members = interop.getMembers(objectWithMembers);
      assert interop.getArraySize(members) < Integer.MAX_VALUE : "long array size not supported";
      int size = (int) interop.getArraySize(members);
      // Final hash code will be put together from member names and member values.
      long[] hashCodes = new long[size * 2];
      int hashCodesIdx = 0;
      for (int i = 0; i < size; i++) {
        String memberName = interop.asString(interop.readArrayElement(members, i));
        hashCodes[hashCodesIdx++] = hashCodeNode.execute(memberName);
        if (interop.isMemberReadable(objectWithMembers, memberName)) {
          Object member = interop.readMember(objectWithMembers, memberName);
          hashCodes[hashCodesIdx++] = hashCodeNode.execute(member);
        } else {
          hashCodes[hashCodesIdx++] = 0;
        }
      }
      return Arrays.hashCode(hashCodes);
    } catch (UnsupportedMessageException | InvalidArrayIndexException | UnknownIdentifierException e) {
      throw new IllegalStateException(
          String.format("An interop object (%s) has probably wrongly specified interop API"
              + " for members.", objectWithMembers),
          e
      );
    }
  }

  @Specialization(
      guards = {"interop.isNull(selfNull)"},
      limit = "3")
  long hashCodeForNull(Object selfNull, @CachedLibrary("selfNull") InteropLibrary interop) {
    return 0;
  }

  @Specialization(guards = "isJavaObject(hostObject)")
  long hashCodeForHostObject(
      Object hostObject,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop) {
    try {
      Object hashCodeRes = interop.invokeMember(hostObject, "hashCode");
      assert interop.fitsInInt(hashCodeRes);
      return interop.asInt(hashCodeRes);
    } catch (UnsupportedMessageException
        | ArityException
        | UnknownIdentifierException
        | UnsupportedTypeException e) {
      throw new IllegalStateException(e);
    }
  }

  /**
   * Every host function has a unique fully qualified name, it is not a lambda.
   * We get the hashcode from the qualified name.
   */
  @TruffleBoundary
  @Specialization(guards = "isJavaFunction(hostFunction)")
  long hashCodeForHostFunction(Object hostFunction,
      @Shared("interop") @CachedLibrary(limit = "10") InteropLibrary interop,
      @Shared("hashCodeNode") @Cached HashCodeNode hashCodeNode) {
    return hashCodeNode.execute(interop.toDisplayString(hostFunction));
  }

  @Fallback
  long fallbackConstant(Object any) {
    return 5343210;
  }

  static boolean isAtom(Object object) {
    return object instanceof Atom;
  }

  boolean isJavaObject(Object object) {
    return EnsoContext.get(this).isJavaPolyglotObject(object);
  }

  boolean isJavaFunction(Object object) {
    return EnsoContext.get(this).isJavaPolyglotFunction(object);
  }
}
