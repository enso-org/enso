package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.*;
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
abstract class WriteToBuilderNode extends Node {

  public abstract void executeWrite(ArrowFixedSizeArrayBuilder receiver, long index, Object value)
      throws UnsupportedTypeException;

  @NeverDefault
  static WriteToBuilderNode build() {
    return WriteToBuilderNodeGen.create();
  }

  @Specialization(guards = "receiver.getUnit() == Date32")
  void doWriteDay(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    validAccess(receiver, index);
    if (iop.isNull(value)) {
      receiver.getBuffer().setNull((int) index);
      return;
    }
    if (!iop.isDate(value)) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
    }
    var at = ArrowFixedArrayDate.typeAdjustedIndex(index, 4);
    long time;
    try {
      time = iop.asDate(value).toEpochDay();
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
    }
    receiver.getBuffer().putInt(at, Math.toIntExact(time));
  }

  @Specialization(guards = {"receiver.getUnit() == Date64"})
  void doWriteMilliseconds(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    validAccess(receiver, index);
    if (iop.isNull(value)) {
      receiver.getBuffer().setNull((int) index);
      return;
    }
    if (!iop.isDate(value) || !iop.isTime(value)) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date and a time");
    }

    var at = ArrowFixedArrayDate.typeAdjustedIndex(index, 8);
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
      receiver.getBuffer().putLong(at, secondsPlusNano);
    } else {
      Instant dateTime;
      try {
        dateTime = instantForOffset(iop.asDate(value), iop.asTime(value), ZoneOffset.UTC);
      } catch (UnsupportedMessageException e) {
        throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
      }
      var secondsPlusNano =
          dateTime.getEpochSecond() * ArrowFixedArrayDate.NANO_DIV + dateTime.getNano();
      receiver.getBuffer().putLong(at, secondsPlusNano);
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

  @Specialization(guards = "receiver.getUnit() == Int8")
  void doWriteByte(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    validAccess(receiver, index);
    if (iop.isNull(value)) {
      receiver.getBuffer().setNull((int) index);
      return;
    }
    if (!iop.fitsInByte(value)) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value does not fit a byte");
    }
    try {
      receiver.getBuffer().put(typeAdjustedIndex(index, receiver.getUnit()), (iop.asByte(value)));
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a byte");
    }
  }

  @Specialization(guards = "receiver.getUnit() == Int16")
  void doWriteShort(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      Object value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    validAccess(receiver, index);
    if (iop.isNull(value)) {
      receiver.getBuffer().setNull((int) index);
      return;
    }
    if (!iop.fitsInShort(value)) {
      throw UnsupportedTypeException.create(
          new Object[] {value}, "value does not fit a 2 byte short");
    }
    try {
      receiver
          .getBuffer()
          .putShort(typeAdjustedIndex(index, receiver.getUnit()), (iop.asShort(value)));
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not a short");
    }
  }

  @Specialization(guards = "receiver.getUnit() == Int32")
  void doWriteInt(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      int value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    validAccess(receiver, index);
    if (iop.isNull(value)) {
      receiver.getBuffer().setNull((int) index);
      return;
    }
    if (!iop.fitsInInt(value)) {
      throw UnsupportedTypeException.create(
          new Object[] {value}, "value does not fit a 4 byte int");
    }
    try {
      receiver.getBuffer().putInt(typeAdjustedIndex(index, receiver.getUnit()), (iop.asInt(value)));
    } catch (UnsupportedMessageException e) {
      throw UnsupportedTypeException.create(new Object[] {value}, "value is not an int");
    }
  }

  @Specialization(guards = "receiver.getUnit() == Int64")
  public static void doWriteLong(
      ArrowFixedSizeArrayBuilder receiver,
      long index,
      long value,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedTypeException {
    validAccess(receiver, index);
    if (iop.isNull(value)) {
      receiver.getBuffer().setNull((int) index);
      return;
    }
    receiver.getBuffer().putLong(typeAdjustedIndex(index, receiver.getUnit()), value);
  }

  @Fallback
  void doWriteOther(ArrowFixedSizeArrayBuilder receiver, long index, Object value)
      throws UnsupportedTypeException {
    throw UnsupportedTypeException.create(new Object[] {index, value}, "unknown type of receiver");
  }

  private static void validAccess(ArrowFixedSizeArrayBuilder receiver, long index)
      throws UnsupportedTypeException {
    if (receiver.isSealed()) {
      throw UnsupportedTypeException.create(
          new Object[] {receiver}, "receiver is not an unsealed buffer");
    }
    if (index >= receiver.getSize() || index < 0) {
      throw UnsupportedTypeException.create(new Object[] {index}, "index is out of range");
    }
  }

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return ArrowFixedArrayDate.typeAdjustedIndex(index, unit.sizeInBytes());
  }
}
