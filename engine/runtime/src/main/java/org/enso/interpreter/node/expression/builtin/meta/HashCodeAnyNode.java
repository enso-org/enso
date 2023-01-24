package org.enso.interpreter.node.expression.builtin.meta;

import com.ibm.icu.text.Normalizer2;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import com.oracle.truffle.api.profiles.LoopConditionProfile;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Arrays;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

/**
 * Implements {@code hash_code} functionality.
 *
 * <p>Make sure that the hashing contract is retained after any modification.
 *
 * <h3>Hashing contract:</h3>
 *
 * <ul>
 *   <li>Whenever two objects are equal ({@code EqualsAnyNode} returns {@code true}), their hashcode
 *       should equal. More formally: {@code For all objects o1, o2: if o1 == o2 then hash(o1) ==
 *       hash(o2)}
 *   <li>Whenever two hash codes are different, their associated objects are different: {@code For all objects
 *       o1, o2: if hash(o1) != hash(o2) then o1 != o2.
 * </ul>
 */
@GenerateUncached
public abstract class HashCodeAnyNode extends Node {

  public static HashCodeAnyNode build() {
    return HashCodeAnyNodeGen.create();
  }

  public abstract long execute(@AcceptsError Object self);

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
    return Long.hashCode(l);
  }

  @Specialization
  long hashCodeForInt(int i) {
    return i;
  }

  @Specialization
  long hashCodeForFloat(float f) {
    return Float.hashCode(f);
  }

  @Specialization
  @TruffleBoundary
  long hashCodeForDouble(double d) {
    if (d % 1.0 != 0.0) {
      return Double.hashCode(d);
    } else {
      if (BigIntegerOps.fitsInLong(d)) {
        return hashCodeForLong(Double.valueOf(d).longValue());
      } else {
        try {
          return BigDecimal.valueOf(d).toBigIntegerExact().hashCode();
        } catch (ArithmeticException e) {
          throw new IllegalStateException(e);
        }
      }
    }
  }

  @Specialization
  @TruffleBoundary
  long hashCodeForBigInteger(EnsoBigInteger bigInteger) {
    return bigInteger.getValue().hashCode();
  }

  @Specialization
  long hashCodeForAtomConstructor(AtomConstructor atomConstructor) {
    return System.identityHashCode(atomConstructor);
  }

  /** How many {@link HashCodeAnyNode} nodes should be created for fields in atoms. */
  static final int hashCodeNodeCountForFields = 10;

  static HashCodeAnyNode[] createHashCodeNodes(int size) {
    HashCodeAnyNode[] nodes = new HashCodeAnyNode[size];
    Arrays.fill(nodes, HashCodeAnyNode.build());
    return nodes;
  }

  @Specialization
  long hashCodeForAtom(
      Atom atom,
      @Cached(value = "createHashCodeNodes(hashCodeNodeCountForFields)", allowUncached = true)
          HashCodeAnyNode[] fieldHashCodeNodes,
      @Cached ConditionProfile isHashCodeCached,
      @Cached ConditionProfile enoughHashCodeNodesForFields,
      @Cached LoopConditionProfile loopProfile) {
    if (isHashCodeCached.profile(atom.getHashCode() != null)) {
      return atom.getHashCode();
    }
    // TODO[PM]: If atom overrides hash_code, call that method (Will be done in a follow-up PR for
    // https://www.pivotaltracker.com/story/show/183945328)
    int fieldsCount = atom.getFields().length;
    Object[] fields = atom.getFields();
    // hashes stores hash codes for all fields, and for constructor.
    int[] hashes = new int[fieldsCount + 1];
    if (enoughHashCodeNodesForFields.profile(fieldsCount <= hashCodeNodeCountForFields)) {
      loopProfile.profileCounted(fieldsCount);
      for (int i = 0; loopProfile.inject(i < fieldsCount); i++) {
        hashes[i] = (int) fieldHashCodeNodes[i].execute(fields[i]);
      }
    } else {
      hashCodeForAtomFieldsUncached(fields, hashes);
    }

    int ctorHashCode = System.identityHashCode(atom.getConstructor());
    hashes[hashes.length - 1] = ctorHashCode;

    int atomHashCode = Arrays.hashCode(hashes);
    atom.setHashCode(atomHashCode);
    return atomHashCode;
  }

  @TruffleBoundary
  private void hashCodeForAtomFieldsUncached(Object[] fields, int[] fieldHashes) {
    for (int i = 0; i < fields.length; i++) {
      fieldHashes[i] = (int) HashCodeAnyNodeGen.getUncached().execute(fields[i]);
    }
  }

  @Specialization(
      guards = {"warnLib.hasWarnings(selfWithWarning)"},
      limit = "3")
  long hashCodeForWarning(
      Object selfWithWarning,
      @CachedLibrary("selfWithWarning") WarningsLibrary warnLib,
      @Cached HashCodeAnyNode hashCodeNode) {
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
  long hashCodeForText(Text text, @CachedLibrary(limit = "3") InteropLibrary interop) {
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
  long hashCodeForString(Object selfStr, @CachedLibrary("selfStr") InteropLibrary interop) {
    String str;
    try {
      str = interop.asString(selfStr);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    Normalizer2 normalizer = Normalizer2.getNFDInstance();
    if (normalizer.isNormalized(str)) {
      return str.hashCode();
    } else {
      return normalizer.normalize(str).hashCode();
    }
  }

  @Specialization(
      guards = {"interop.hasArrayElements(selfArray)"},
      limit = "3")
  long hashCodeForArray(
      Object selfArray,
      @CachedLibrary("selfArray") InteropLibrary interop,
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached("createCountingProfile()") LoopConditionProfile loopProfile) {
    try {
      long arraySize = interop.getArraySize(selfArray);
      loopProfile.profileCounted(arraySize);
      int[] elemHashCodes = new int[(int) arraySize];
      for (int i = 0; loopProfile.inject(i < arraySize); i++) {
        if (interop.isArrayElementReadable(selfArray, i)) {
          elemHashCodes[i] = (int) hashCodeNode.execute(interop.readArrayElement(selfArray, i));
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
  @Specialization(guards = "interop.hasHashEntries(selfMap)")
  long hashCodeForMap(
      Object selfMap,
      @CachedLibrary(limit = "5") InteropLibrary interop,
      @Cached HashCodeAnyNode hashCodeNode) {
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

  @Specialization(
      guards = {"interop.isNull(selfNull)"},
      limit = "3")
  long hashCodeForNull(Object selfNull, @CachedLibrary("selfNull") InteropLibrary interop) {
    return 0;
  }

  @Specialization(guards = "isHostObject(hostObject)")
  long hashCodeForHostObject(
      Object hostObject, @CachedLibrary(limit = "3") InteropLibrary interop) {
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

  @TruffleBoundary
  boolean isHostObject(Object object) {
    return EnsoContext.get(this).getEnvironment().isHostObject(object);
  }
}
