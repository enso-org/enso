package org.enso.interpreter.node.expression.builtin.ordering;

import com.ibm.icu.text.Normalizer;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.time.Duration;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import org.enso.interpreter.dsl.AcceptsError;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.WarningsLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(
    type = "Comparable",
    name = "less_than_builtin",
    description = """
        Returns true if self is less than `other`. Or throw an error if the values are
        not comparable.
        """
)
@GenerateUncached
public abstract class LessThanNode extends Node {

  public static LessThanNode build() {
    return LessThanNodeGen.create();
  }

  public abstract Object execute(@AcceptsError Object left, @AcceptsError Object other);

  @Specialization
  boolean lessIntegers(int i, int j) {
    return i < j;
  }

  @Specialization
  boolean lessBools(boolean b1, boolean b2) {
    return !b1 && b2;
  }

  @Specialization
  boolean lessLongs(long l1, long l2) {
    return l1 < l2;
  }

  @Specialization
  boolean lessDoubles(double self, double other) {
    return self < other;
  }

  @Specialization
  boolean lessLongDouble(long self, double other) {
    return (double) self < other;
  }

  @Specialization
  boolean lessDoubleLong(double self, long other) {
    return self < (double) other;
  }

  @Specialization
  boolean lessIntLong(int self, long other) {
    return (long) self < other;
  }

  @Specialization
  boolean lessLongInt(long self, int other) {
    return self < (long) other;
  }


  @Specialization
  boolean lessIntDouble(int self, double other) {
    return (double) self < other;
  }

  @Specialization
  boolean lessDoubleInt(double self, int other) {
    return self < (double) other;
  }

  @Specialization
  @TruffleBoundary
  boolean lessBigInt(EnsoBigInteger self, EnsoBigInteger other) {
    return self.getValue().compareTo(other.getValue()) < 0;
  }

  @Specialization
  @TruffleBoundary
  boolean lessBitIntDouble(EnsoBigInteger self, double other) {
    return self.doubleValue() < other;
  }

  @Specialization
  @TruffleBoundary
  boolean lessDoubleBigInt(double self, EnsoBigInteger other) {
    return self < other.doubleValue();
  }

  /**
   * If one of the objects has warnings attached, just treat it as an object without any
   * warnings.
   */
  @Specialization(guards = {
      "selfWarnLib.hasWarnings(selfWithWarnings) || otherWarnLib.hasWarnings(otherWithWarnings)"
  }, limit = "3")
  Object lessWithWarnings(Object selfWithWarnings, Object otherWithWarnings,
      @CachedLibrary("selfWithWarnings") WarningsLibrary selfWarnLib,
      @CachedLibrary("otherWithWarnings") WarningsLibrary otherWarnLib,
      @Cached LessThanNode lessThanNode
  ) {
    try {
      Object self =
          selfWarnLib.hasWarnings(selfWithWarnings) ? selfWarnLib.removeWarnings(selfWithWarnings)
              : selfWithWarnings;
      Object other =
          otherWarnLib.hasWarnings(otherWithWarnings) ? otherWarnLib.removeWarnings(otherWithWarnings)
              : otherWithWarnings;
      return lessThanNode.execute(self, other);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Specialization(limit = "3")
  boolean lessTexts(Text selfText, Text otherText,
      @CachedLibrary("selfText") InteropLibrary selfInterop,
      @CachedLibrary("otherText") InteropLibrary otherInterop) {
    if (selfText.is_normalized() && otherText.is_normalized()) {
      return selfText.toString().compareTo(otherText.toString()) < 0;
    } else {
      return lessInteropStrings(selfText, otherText, selfInterop, otherInterop);
    }
  }

  @Specialization(
      guards = {
          "selfInterop.isString(selfStr)",
          "otherInterop.isString(otherStr)"
      },
      limit = "5"
  )
  @TruffleBoundary
  boolean lessInteropStrings(Object selfStr, Object otherStr,
      @CachedLibrary("selfStr") InteropLibrary selfInterop,
      @CachedLibrary("otherStr") InteropLibrary otherInterop) {
    String selfJavaString;
    String otherJavaString;
    try {
      selfJavaString = selfInterop.asString(selfStr);
      otherJavaString = otherInterop.asString(otherStr);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    return Normalizer.compare(
        selfJavaString,
        otherJavaString,
        Normalizer.FOLD_CASE_DEFAULT
    ) < 0;
  }

  @Specialization(guards = {
      "selfInterop.isBoolean(selfBoolean)",
      "otherInterop.isBoolean(otherBoolean)"
  }, limit = "3")
  boolean lessInteropBoolean(
      Object selfBoolean,
      Object otherBoolean,
      @CachedLibrary("selfBoolean") InteropLibrary selfInterop,
      @CachedLibrary("otherBoolean") InteropLibrary otherInterop
  ) {
    try {
      return !selfInterop.asBoolean(selfBoolean) && otherInterop.asBoolean(otherBoolean);
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
  boolean lessInteropZonedDateTimes(Object selfZonedDateTime, Object otherZonedDateTime,
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
      return self.compareTo(other) < 0;
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
  boolean lessInteropDateTimes(Object selfDateTime, Object otherDateTime,
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
      return self.isBefore(other);
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
  boolean lessInteropDates(Object selfDate, Object otherDate,
      @CachedLibrary("selfDate") InteropLibrary selfInterop,
      @CachedLibrary("otherDate") InteropLibrary otherInterop) {
    try {
      return selfInterop.asDate(selfDate).isBefore(
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
  boolean lessInteropTimes(Object selfTime, Object otherTime,
      @CachedLibrary("selfTime") InteropLibrary selfInterop,
      @CachedLibrary("otherTime") InteropLibrary otherInterop) {
    try {
      return selfInterop.asTime(selfTime).isBefore(
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
  boolean lessInteropDuration(Object selfDuration, Object otherDuration,
      @CachedLibrary("selfDuration") InteropLibrary selfInterop,
      @CachedLibrary("otherDuration") InteropLibrary otherInterop) {
    try {
      Duration selfDur = selfInterop.asDuration(selfDuration);
      Duration otherDur = otherInterop.asDuration(otherDuration);
      return selfDur.compareTo(otherDur) < 0;
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @Fallback
  Object fallback(Object left, Object right) {
    var typeError = EnsoContext.get(this).getBuiltins().error().makeTypeError(left, right, "right");
    return DataflowError.withoutTrace(typeError, this);
  }
}
