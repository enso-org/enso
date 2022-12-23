package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.LoopConditionProfile;
import com.oracle.truffle.api.profiles.ValueProfile;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.util.Arrays;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.UnresolvedConversion;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.error.WarningsLibrary;

/**
 * Hashing contract:
 * Whenever two objects are equal ({@code EqualsAnyNode} returns {@code true}), their hashcode
 * should equal.
 * Note that instances of different types are always not equal, but they might have same hashcode.
 * More formally:
 * {@code For all types T, for all instances i1, i2 of type T it holds: i1 == i2 iff hash(i1) == hash(i2)}
 */
@BuiltinMethod(
    type = "Any",
    name = "hash_code",
    description = "Returns has code of the object."
)
@GenerateUncached
public abstract class HashCodeAnyNode extends Node {

  public static HashCodeAnyNode build() {
    return HashCodeAnyNodeGen.create();
  }

  public abstract long execute(@AcceptsError Object self);

  /** Specializations for primitive values **/

  @Specialization
  long hashCodeForLong(long l) {
    return l;
  }

  @Specialization
  long hashCodeForInt(int i) {
    return i;
  }

  @Specialization
  long hashCodeForDouble(double d) {
    // TODO: More precise conversion?
    return Double.valueOf(d).longValue();
  }

  @Specialization
  long hashCodeForUnresolvedSymbol(UnresolvedSymbol selfSymbol) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Specialization
  long hashCodeForUnresolvedConversion(UnresolvedConversion selfConversion) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Specialization(guards = {
      "warnLib.hasWarnings(selfWithWarning)"
  }, limit = "3")
  long hashCodeForWarning(Object selfWithWarning,
      @CachedLibrary("selfWithWarning") WarningsLibrary warnLib,
      @Cached HashCodeAnyNode hashCodeNode
  ) {
    try {
      return hashCodeNode.execute(
          warnLib.removeWarnings(selfWithWarning)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  /** Specializations for interop values **/

  @Specialization(guards = {
      "interop.isBoolean(selfBool)"
  }, limit = "3")
  long hashCodeForBooleanInterop(Object selfBool,
      @CachedLibrary("selfBool") InteropLibrary interop) {
    try {
      return Boolean.hashCode(
          interop.asBoolean(selfBool)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.isMetaObject(selfMeta)"
  }, limit = "3")
  long hashCodeForMetaInterop(Object selfMeta,
      @CachedLibrary("selfMeta") InteropLibrary interop,
      @Cached HashCodeAnyNode hashCodeNode) {
    try {
      return hashCodeNode.execute(
          interop.getMetaQualifiedName(selfMeta)
      );
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "!interop.isDate(selfTimeZone)",
      "!interop.isTime(selfTimeZone)",
      "interop.isTimeZone(selfTimeZone)",
  }, limit = "3")
  long hashCodeForTimeZoneInterop(Object selfTimeZone,
      @CachedLibrary("selfTimeZone") InteropLibrary interop) {
    try {
      return interop.asTimeZone(selfTimeZone).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.isDate(selfZonedDateTime)",
      "interop.isTime(selfZonedDateTime)",
      "interop.isTimeZone(selfZonedDateTime)",
  }, limit = "3")
  long hashCodeForZonedDateTimeInterop(Object selfZonedDateTime,
      @CachedLibrary("selfZonedDateTime") InteropLibrary interop) {
    try {
      return ZonedDateTime.of(
          interop.asDate(selfZonedDateTime),
          interop.asTime(selfZonedDateTime),
          interop.asTimeZone(selfZonedDateTime)
      ).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.isDate(selfDateTime)",
      "interop.isTime(selfDateTime)",
      "!interop.isTimeZone(selfDateTime)",
  }, limit = "3")
  long hashCodeForDateTimeInterop(Object selfDateTime,
      @CachedLibrary("selfDateTime") InteropLibrary interop) {
    try {
      return LocalDateTime.of(
          interop.asDate(selfDateTime),
          interop.asTime(selfDateTime)
      ).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "!interop.isDate(selfTime)",
      "interop.isTime(selfTime)",
      "!interop.isTimeZone(selfTime)",
  }, limit = "3")
  long hashCodeForTimeInterop(Object selfTime,
      @CachedLibrary("selfTime") InteropLibrary interop) {
    try {
      return interop.asTime(selfTime).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.isDate(selfDate)",
      "!interop.isTime(selfDate)",
      "!interop.isTimeZone(selfDate)",
  }, limit = "3")
  long hashCodeForDateInterop(Object selfDate,
      @CachedLibrary("selfDate") InteropLibrary interop) {
    try {
      return interop.asDate(selfDate).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.isDuration(selfDuration)",
  }, limit = "3")
  long hashCodeForDurationInterop(Object selfDuration,
      @CachedLibrary("selfDuration") InteropLibrary interop) {
    try {
      return interop.asDuration(selfDuration).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.isString(selfStr)"
  }, limit = "3")
  long hashCodeForString(Object selfStr,
      @CachedLibrary("selfStr") InteropLibrary interop) {
    try {
      return interop.asString(selfStr).hashCode();
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.hasArrayElements(selfArray)"
  }, limit = "3")
  long hashCodeForArray(Object selfArray,
      @CachedLibrary("selfArray") InteropLibrary interop,
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached("createCountingProfile()") LoopConditionProfile loopProfile) {
    try {
      long arraySize = interop.getArraySize(selfArray);
      loopProfile.profileCounted(arraySize);
      int[] elemHashCodes = new int[(int) arraySize];
      for (int i = 0; loopProfile.inject(i < arraySize); i++) {
        if (interop.isArrayElementReadable(selfArray, i)) {
          elemHashCodes[i] = (int) hashCodeNode.execute(
              interop.readArrayElement(selfArray, i)
          );
        }
      }
      return Arrays.hashCode(elemHashCodes);
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.hasIdentity(selfWithIdentity)"
  }, limit = "3")
  long hashCodeForInteropWithIdentity(
      Object selfWithIdentity,
      @CachedLibrary("selfWithIdentity") InteropLibrary interop,
      @Cached("createClassProfile()") ValueProfile objectClassProfile) {
    try {
      return interop.identityHashCode(objectClassProfile.profile(selfWithIdentity));
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(guards = {
      "interop.isNull(selfNull)"
  }, limit = "3")
  long hashCodeForNull(Object selfNull,
      @CachedLibrary("selfNull") InteropLibrary interop
  ) {
    return 0;
  }

  @TruffleBoundary
  @Fallback
  long hashCodeGeneric(Object self) {
    return self.hashCode();
  }
}
