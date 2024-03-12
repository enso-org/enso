package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
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

  private WriteToArray writeToArray;

  public ArrowFixedSizeArrayBuilder(int size, LogicalLayout unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = ByteBufferDirect.forSize(size, unit);
    this.index = 0;
    this.sealed = false;
    this.writeToArray =
        switch (unit) {
          case Date32 -> new DayWriteToArray();
          case Date64 -> new MillisecondsWriteToArray();
          case Int8 -> new Int8WriteToArray();
          case Int16 -> new Int16WriteToArray();
          case Int32 -> new Int32WriteToArray();
          case Int64 -> new Int64WriteToArray();
        };
  }

  @ExportMessage
  public boolean hasArrayElements() {
    return false;
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
  Object invokeMember(String name, Object[] args, @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedMessageException, UnknownIdentifierException, UnsupportedTypeException {
    switch (name) {
      case "build":
        sealed = true;
        return switch (unit) {
          case Date32, Date64 -> new ArrowFixedArrayDate(buffer, size, unit);
          case Int8, Int16, Int32, Int64 -> new ArrowFixedArrayInt(buffer, size, unit);
        };
      case "append":
        if (sealed) {
          throw UnsupportedMessageException.create();
        }
        var current = index;
        try {
          writeToArray.write(this, current, args[0], iop);
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
  final void writeArrayElement(long index, Object value)
      throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {}

  abstract class WriteToArray {
    abstract void write(
        ArrowFixedSizeArrayBuilder receiver, long index, Object value, InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException;

    void validAccess(ArrowFixedSizeArrayBuilder receiver, long index)
        throws InvalidArrayIndexException, UnsupportedMessageException {
      if (receiver.sealed) {
        throw UnsupportedMessageException.create();
      }
      if (index >= receiver.size || index < 0) {
        throw InvalidArrayIndexException.create(index);
      }
    }
  }

  private final class DayWriteToArray extends WriteToArray {

    @Override
    void write(ArrowFixedSizeArrayBuilder receiver, long index, Object value, InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {
      validAccess(receiver, index);
      if (iop.isNull(value)) {
        receiver.buffer.setNull((int) index);
        return;
      }
      if (!iop.isDate(value)) {
        throw UnsupportedTypeException.create(new Object[] {value}, "value is not a date");
      }
      var at = ArrowFixedArrayDate.typeAdjustedIndex(index, 4);
      var time = iop.asDate(value).toEpochDay();
      receiver.buffer.putInt(at, Math.toIntExact(time));
    }
  }

  private final class MillisecondsWriteToArray extends WriteToArray {

    @Override
    void write(ArrowFixedSizeArrayBuilder receiver, long index, Object value, InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {
      validAccess(receiver, index);
      if (iop.isNull(value)) {
        receiver.buffer.setNull((int) index);
        return;
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
  }

  private final class Int8WriteToArray extends WriteToArray {

    @Override
    void write(ArrowFixedSizeArrayBuilder receiver, long index, Object value, InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {
      validAccess(receiver, index);
      if (iop.isNull(value)) {
        receiver.buffer.setNull((int) index);
        return;
      }
      if (!iop.fitsInByte(value)) {
        throw UnsupportedTypeException.create(new Object[] {value}, "value does not fit a byte");
      }
      receiver.buffer.put(typeAdjustedIndex(index, receiver.unit), (iop.asByte(value)));
    }
  }

  private final class Int16WriteToArray extends WriteToArray {

    @Override
    void write(ArrowFixedSizeArrayBuilder receiver, long index, Object value, InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {
      validAccess(receiver, index);
      if (iop.isNull(value)) {
        receiver.buffer.setNull((int) index);
        return;
      }
      if (!iop.fitsInShort(value)) {
        throw UnsupportedTypeException.create(
            new Object[] {value}, "value does not fit a 2 byte short");
      }
      receiver.buffer.putShort(typeAdjustedIndex(index, receiver.unit), (iop.asShort(value)));
    }
  }

  private final class Int32WriteToArray extends WriteToArray {

    @Override
    void write(ArrowFixedSizeArrayBuilder receiver, long index, Object value, InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {
      validAccess(receiver, index);
      if (iop.isNull(value)) {
        receiver.buffer.setNull((int) index);
        return;
      }
      if (!iop.fitsInInt(value)) {
        throw UnsupportedTypeException.create(
            new Object[] {value}, "value does not fit a 4 byte int");
      }
      receiver.buffer.putInt(typeAdjustedIndex(index, receiver.unit), (iop.asInt(value)));
    }
  }

  private final class Int64WriteToArray extends WriteToArray {

    @Override
    void write(ArrowFixedSizeArrayBuilder receiver, long index, Object value, InteropLibrary iop)
        throws UnsupportedMessageException, UnsupportedTypeException, InvalidArrayIndexException {
      validAccess(receiver, index);
      if (iop.isNull(value)) {
        receiver.buffer.setNull((int) index);
        return;
      }
      if (!iop.fitsInLong(value)) {
        throw UnsupportedTypeException.create(
            new Object[] {value}, "value does not fit a 8 byte long");
      }
      receiver.buffer.putLong(typeAdjustedIndex(index, receiver.unit), (iop.asLong(value)));
    }
  }

  @ExportMessage
  long getArraySize() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  Object readArrayElement(long index)
      throws UnsupportedMessageException, InvalidArrayIndexException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementModifiable(long index) {
    return false;
  }

  @ExportMessage
  boolean isArrayElementInsertable(long index) {
    return false;
  }

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return ArrowFixedArrayDate.typeAdjustedIndex(index, unit.sizeInBytes());
  }
}
