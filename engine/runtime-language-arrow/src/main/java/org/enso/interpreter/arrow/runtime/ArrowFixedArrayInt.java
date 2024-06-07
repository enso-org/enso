package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.InlinedExactClassProfile;
import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedArrayInt implements TruffleObject {
  private final int size;
  private final ByteBufferDirect buffer;
  private final LogicalLayout unit;

  public ArrowFixedArrayInt(ByteBufferDirect buffer, int size, LogicalLayout unit) {
    this.size = size;
    this.unit = unit;
    this.buffer = buffer;
  }

  public LogicalLayout getUnit() {
    return unit;
  }

  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  Object getIterator(
      @Cached(value = "this.getUnit()", allowUncached = true) LogicalLayout cachedUnit)
      throws UnsupportedMessageException {
    return switch (cachedUnit) {
      case Int64 -> new LongIterator(buffer.dataBuffer, cachedUnit.sizeInBytes());
      default -> new GenericIterator(this);
    };
  }

  @ExportMessage
  boolean hasIterator() {
    return true;
  }

  @ExportMessage
  @ImportStatic(LogicalLayout.class)
  static class ReadArrayElement {
    @Specialization(guards = "receiver.getUnit() == Int8")
    public static Object doByte(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.get(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int16")
    public static Object doShort(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getShort(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int32")
    public static Object doInt(ArrowFixedArrayInt receiver, long index)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, receiver.unit, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getInt(at);
    }

    @Specialization(guards = "receiver.getUnit() == Int64")
    public static Object doLong(
        ArrowFixedArrayInt receiver,
        long index,
        @Bind("$node") Node node,
        @CachedLibrary("receiver") InteropLibrary iop,
        @Cached InlinedExactClassProfile bufferClazz)
        throws UnsupportedMessageException, InvalidArrayIndexException {
      var at = adjustedIndex(receiver.buffer, LogicalLayout.Int64, receiver.size, index);
      if (receiver.buffer.isNull((int) index)) {
        return NullValue.get();
      }
      return receiver.buffer.getLong(at, iop, bufferClazz);
    }
  }

  public static int adjustedIndex(ByteBufferDirect buffer, LogicalLayout unit, int size, long index)
      throws InvalidArrayIndexException {
    if (index >= size || index < 0) {
      throw InvalidArrayIndexException.create(index);
    }
    return typeAdjustedIndex(index, unit);
  }

  @ExportMessage
  long getArraySize() {
    return size;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size && !buffer.isNull((int) index);
  }

  private static int typeAdjustedIndex(long index, SizeInBytes unit) {
    return Math.toIntExact(index * unit.sizeInBytes());
  }

  @ExportLibrary(InteropLibrary.class)
  static final class LongIterator implements TruffleObject {
    private int at;
    private final ByteBuffer buffer;
    @NeverDefault final int step;

    LongIterator(ByteBuffer buffer, int step) {
      assert step != 0;
      this.buffer = buffer;
      this.step = step;
    }

    @ExportMessage
    Object getIteratorNextElement(
        @Bind("$node") Node node,
        @Cached("this.step") int step,
        @Cached InlinedExactClassProfile bufferTypeProfile)
        throws StopIterationException {
      var buf = bufferTypeProfile.profile(node, buffer);
      try {
        var res = buf.getLong(at);
        at += step;
        return res;
      } catch (BufferOverflowException ex) {
        CompilerDirectives.transferToInterpreter();
        throw StopIterationException.create();
      }
    }

    @ExportMessage
    boolean isIterator() {
      return true;
    }

    @ExportMessage
    boolean hasIteratorNextElement() throws UnsupportedMessageException {
      return at < buffer.limit();
    }
  }

  @ExportLibrary(InteropLibrary.class)
  static final class GenericIterator implements TruffleObject {
    private int at;
    private final TruffleObject array;

    GenericIterator(TruffleObject array) {
      assert InteropLibrary.getUncached().hasArrayElements(array);
      this.array = array;
    }

    TruffleObject array() {
      return array;
    }

    @ExportMessage(limit = "3")
    Object getIteratorNextElement(@CachedLibrary("this.array()") InteropLibrary iop)
        throws StopIterationException {
      try {
        var res = iop.readArrayElement(array, at);
        at++;
        return res;
      } catch (UnsupportedMessageException | InvalidArrayIndexException ex) {
        throw StopIterationException.create();
      }
    }

    @ExportMessage
    boolean isIterator() {
      return true;
    }

    @ExportMessage(limit = "3")
    boolean hasIteratorNextElement(@CachedLibrary("this.array()") InteropLibrary iop)
        throws UnsupportedMessageException {
      return at < iop.getArraySize(array);
    }
  }
}
