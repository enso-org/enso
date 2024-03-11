package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.time.*;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedSizeArrayBuilder implements TruffleObject {
  private final ByteBufferDirect buffer;
  private final LogicalLayout unit;
  private final int size;
  private int index;

  private boolean sealed;

  public ArrowFixedSizeArrayBuilder(int size, LogicalLayout unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = ByteBufferDirect.forSize(size, unit);
    this.index = 0;
    this.sealed = false;
  }

  public LogicalLayout getUnit() {
    return unit;
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  public boolean hasMembers() {
    return true;
  }

  @ExportMessage
  public boolean isMemberInvocable(String member) {
    return switch (member) {
      case "append" -> !this.sealed;
      case "build" -> true;
      default -> false;
    };
  }

  @ExportMessage
  Object getMembers(boolean includeInternal) throws UnsupportedMessageException {
    return this;
  }

  @ExportMessage
  Object invokeMember(
      String name,
      Object[] args,
      @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedMessageException, UnknownIdentifierException, UnsupportedTypeException {
    switch (name) {
      case "build":
        sealed = true;
        return switch (unit) {
          case Date32, Date64 -> new ArrowFixedArrayDate(buffer, unit);
          case Int8, Int16, Int32, Int64 -> new ArrowFixedArrayInt(buffer, unit);
        };
      case "append":
        if (sealed) {
          throw UnsupportedMessageException.create();
        }
        var current = index;
        try {
          iop.writeArrayElement(this, current, args[0]);
        } catch (InvalidArrayIndexException e) {
          throw UnsupportedMessageException.create(e);
        }
        index += 1;
        return NullValue.get();
      default:
        throw UnknownIdentifierException.create(name);
    }
  }

  @ExportMessage
  @ImportStatic(LogicalLayout.class)
  static class ReadArrayElement {

    @Specialization(guards = "receiver.getUnit() == Date32")
    static Object doDay(ArrowFixedSizeArrayBuilder receiver, long index)
        throws UnsupportedMessageException {
      return ArrowFixedArrayDate.readDay(receiver.buffer, index);
    }

    @Specialization(guards = "receiver.getUnit() == Date64")
    static Object doMilliseconds(ArrowFixedSizeArrayBuilder receiver, long index)
        throws UnsupportedMessageException {
      return ArrowFixedArrayDate.readMilliseconds(receiver.buffer, index);
    }

    @Specialization(guards = "receiver.getUnit() == Int8")
    public static Object doByte(ArrowFixedSizeArrayBuilder receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at =
          ArrowFixedArrayInt.adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.get(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int16")
    public static Object doShort(ArrowFixedSizeArrayBuilder receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at =
          ArrowFixedArrayInt.adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getShort(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int32")
    public static Object doInt(ArrowFixedSizeArrayBuilder receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at =
          ArrowFixedArrayInt.adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getInt(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int64")
    public static Object doLong(ArrowFixedSizeArrayBuilder receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at =
          ArrowFixedArrayInt.adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getLong(at);
    }
  }

  @ExportMessage
  @ImportStatic(LogicalLayout.class)
  static class WriteArrayElement {

    @Specialization(guards = "receiver.getUnit() == Date32")
    static void doDay(
        ArrowFixedSizeArrayBuilder receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException {
      if (receiver.sealed) {
        throw UnsupportedMessageException.create();
      }
      if (!iop.isDate(value)) {
        throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
      }
      var at = ArrowFixedArrayDate.typeAdjustedIndex(index, 4);
      var time = iop.asDate(value).toEpochDay();
      receiver.buffer.putInt(at, Math.toIntExact(time));
    }

    @Specialization(guards = {"receiver.getUnit() == Date64", "!iop.isNull(value)"})
    static void doMilliseconds(
        ArrowFixedSizeArrayBuilder receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException {
      if (receiver.sealed) {
        throw UnsupportedMessageException.create();
      }
      if (!iop.isDate(value) || !iop.isTime(value)) {
        throw UnsupportedTypeException.create(
            new Object[] {value}, "value is not a date and a time");
      }

      var at = ArrowFixedArrayDate.typeAdjustedIndex(index, 8);
      if (iop.isTimeZone(value)) {
        var zoneDateTimeInstant =
            instantForZone(
                iop.asDate(value),
                iop.asTime(value),
                iop.asTimeZone(value),
                ArrowFixedArrayDate.UTC);
        var secondsPlusNano =
            zoneDateTimeInstant.getEpochSecond() * ArrowFixedArrayDate.NANO_DIV
                + zoneDateTimeInstant.getNano();
        receiver.buffer.putLong(at, secondsPlusNano);
      } else {
        var dateTime = instantForOffset(iop.asDate(value), iop.asTime(value), ZoneOffset.UTC);
        var secondsPlusNano =
            dateTime.getEpochSecond() * ArrowFixedArrayDate.NANO_DIV + dateTime.getNano();
        receiver.buffer.putLong(at, secondsPlusNano);
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

    @Specialization(guards = "iop.isNull(value)")
    static void doNull(
        ArrowFixedSizeArrayBuilder receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      if (receiver.sealed) {
        throw UnsupportedMessageException.create();
      }
      receiver.buffer.setNull((int) index);
    }

    @Specialization(guards = "receiver.getUnit() == Int8")
    public static void doByte(
        ArrowFixedSizeArrayBuilder receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, InvalidArrayIndexException, UnsupportedTypeException {
      validAccess(receiver, index);
      if (!iop.fitsInByte(value)) {
        throw UnsupportedTypeException.create(new Object[] {value}, "value does not fit a byte");
      }
      receiver.buffer.put(typeAdjustedIndex(index, receiver.unit), (iop.asByte(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Int16")
    public static void doShort(
        ArrowFixedSizeArrayBuilder receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, InvalidArrayIndexException, UnsupportedTypeException {
      validAccess(receiver, index);
      if (!iop.fitsInShort(value)) {
        throw UnsupportedTypeException.create(
            new Object[] {value}, "value does not fit a 2 byte short");
      }
      receiver.buffer.putShort(typeAdjustedIndex(index, receiver.unit), (iop.asShort(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Int32")
    public static void doInt(
        ArrowFixedSizeArrayBuilder receiver,
        long index,
        int value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, InvalidArrayIndexException, UnsupportedTypeException {
      validAccess(receiver, index);
      if (!iop.fitsInInt(value)) {
        throw UnsupportedTypeException.create(
            new Object[] {value}, "value does not fit a 4 byte int");
      }
      receiver.buffer.putInt(typeAdjustedIndex(index, receiver.unit), (iop.asInt(value)));
    }

    @Specialization(guards = "receiver.getUnit() == Int64")
    public static void doLong(ArrowFixedSizeArrayBuilder receiver, long index, long value)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      validAccess(receiver, index);
      receiver.buffer.putLong(typeAdjustedIndex(index, receiver.unit), value);
    }

    @Fallback
    public static void doOther(
        ArrowFixedSizeArrayBuilder receiver,
        long index,
        Object value,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedTypeException {
      throw UnsupportedTypeException.create(
          new Object[] {index, value}, "unknown type of receiver");
    }

    private static void validAccess(ArrowFixedSizeArrayBuilder receiver, long index)
        throws InvalidArrayIndexException, UnsupportedMessageException {
      if (receiver.sealed) {
        throw UnsupportedMessageException.create();
      }
      if (index >= receiver.size || index < 0) {
        throw InvalidArrayIndexException.create(index);
      }
    }
  }

  @ExportMessage
  long getArraySize() {
    return size;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size && !buffer.isNull((int) index);
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return !sealed && index >= 0 && index < size;
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return !sealed && index >= 0 && index < size;
  }

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return ArrowFixedArrayDate.typeAdjustedIndex(index, unit.sizeInBytes());
  }
}
