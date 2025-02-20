package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.GenerateInline;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import org.enso.interpreter.arrow.LogicalLayout;

@ImportStatic(LogicalLayout.class)
@GenerateUncached
@GenerateInline(value = false)
abstract class ValueToNumberNode extends Node {
  /**
   * Converts {@code value} to a suitable representation to be stored in an appropriate
   * DirectBuffer.
   *
   * @param unit type of layout
   * @param value a value to convert
   * @return byte, short, int or long
   * @throws UnsupportedTypeException if the conversion isn't possible
   */
  abstract Number executeAdjust(LogicalLayout unit, Object value) throws UnsupportedTypeException;

  @NeverDefault
  static ValueToNumberNode build() {
    return ValueToNumberNodeGen.create();
  }

  @NeverDefault
  static ValueToNumberNode getUncached() {
    return ValueToNumberNodeGen.getUncached();
  }

  @Specialization(guards = "unit == Date32")
  Integer doDay(
      LogicalLayout unit,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    if (!iop.isDate(value)) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
    }
    long time;
    try {
      time = iop.asDate(value).toEpochDay();
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
    }
    return Math.toIntExact(time);
  }

  @Specialization(guards = {"unit == Date64"})
  Long doMilliseconds(
      LogicalLayout unit,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    if (!iop.isDate(value) || !iop.isTime(value)) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date and a time");
    }

    if (iop.isTimeZone(value)) {
      Instant zoneDateTimeInstant;
      try {
        zoneDateTimeInstant =
            instantForZone(
                iop.asDate(value),
                iop.asTime(value),
                iop.asTimeZone(value),
                ArrowFixedArrayDate.UTC);
      } catch (UnsupportedMessageException e) {
        throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
      }
      var secondsPlusNano =
          zoneDateTimeInstant.getEpochSecond() * ArrowFixedArrayDate.NANO_DIV
              + zoneDateTimeInstant.getNano();
      return secondsPlusNano;
    } else {
      Instant dateTime;
      try {
        dateTime = instantForOffset(iop.asDate(value), iop.asTime(value), ZoneOffset.UTC);
      } catch (UnsupportedMessageException e) {
        throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
      }
      var secondsPlusNano =
          dateTime.getEpochSecond() * ArrowFixedArrayDate.NANO_DIV + dateTime.getNano();
      return secondsPlusNano;
    }
  }

  @CompilerDirectives.TruffleBoundary
  private static Instant instantForZone(
      LocalDate date, LocalTime time, ZoneId zone, ZoneId target) {
    return date.atTime(time).atZone(zone).withZoneSameLocal(target).toInstant();
  }

  @CompilerDirectives.TruffleBoundary
  private static Instant instantForOffset(LocalDate date, LocalTime time, ZoneOffset offset) {
    return date.atTime(time).toInstant(offset);
  }

  @Specialization(guards = "unit == Int8")
  Byte doByte(
      LogicalLayout unit,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    if (!iop.fitsInByte(value)) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value does not fit a byte");
    }
    try {
      return iop.asByte(value);
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a byte");
    }
  }

  @Specialization(guards = "unit == Int16")
  Short doShort(
      LogicalLayout unit,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    if (!iop.fitsInShort(value)) {
      throw UnsupportedTypeException.create(
          new Object[] {value}, "value does not fit a 2 byte short");
    }
    try {
      return iop.asShort(value);
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a short");
    }
  }

  @Specialization(guards = "unit == Int32")
  Integer doInt(
      LogicalLayout unit,
      int value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    if (!iop.fitsInInt(value)) {
      throw UnsupportedTypeException.create(
          new Object[] {value}, "value does not fit a 4 byte int");
    }
    try {
      return iop.asInt(value);
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not an int");
    }
  }

  @Specialization(guards = "unit == Int64")
  static Long doLong(
      LogicalLayout unit,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    if (!iop.fitsInLong(value)) {
      throw UnsupportedTypeException.create(
          new Object[] {value}, "value does not fit a 8 byte int");
    }
    try {
      return iop.asLong(value);
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a long");
    }
  }

  @Fallback
  Number doOther(LogicalLayout unit, Object value) throws UnsupportedTypeException {
    throw UnsupportedTypeException.create(new Object[] {unit, value}, "unknown type");
  }
}
