package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import org.enso.interpreter.arrow.LogicalLayout;
import org.enso.interpreter.arrow.util.MemoryUtil;

@ExportLibrary(InteropLibrary.class)
public class ArrowCastToFixedSizeArrayFactory implements TruffleObject {

  private final LogicalLayout logicalLayout;

  public ArrowCastToFixedSizeArrayFactory(LogicalLayout logicalLayout) {
    this.logicalLayout = logicalLayout;
  }

  @ExportMessage
  public boolean isExecutable() {
    return true;
  }

  public LogicalLayout getLayout() {
    return logicalLayout;
  }

  @ExportMessage
  @ImportStatic(LogicalLayout.class)
  static class Execute {
    @Specialization(guards = "receiver.getLayout() == Date32")
    static Object doDate32(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = ArrowFixedArrayDate.DateUnit.Day;
      return new ArrowFixedArrayDate(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Date64")
    static Object doDate64(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = ArrowFixedArrayDate.DateUnit.Millisecond;
      return new ArrowFixedArrayDate(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int8")
    static Object doInt8(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte1;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int16")
    static Object doInt16(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte2;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int32")
    static Object doInt32(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte4;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int64")
    static Object doInt64(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte8;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @CompilerDirectives.TruffleBoundary
    private static ByteBufferDirect pointer(Object[] args, InteropLibrary interop, SizeInBytes unit)
        throws ArityException, UnsupportedTypeException, UnsupportedMessageException {
      if (args.length < 2) {
        throw ArityException.create(2, 3, args.length);
      }
      if (!interop.isNumber(args[0]) || !interop.fitsInLong(args[0])) {
        throw UnsupportedTypeException.create(
            new Object[] {args[0]}, "Address of Arrow vector is invalid");
      }
      if (!interop.isNumber(args[1]) || !interop.fitsInInt(args[1])) {
        throw UnsupportedTypeException.create(
            new Object[] {args[0]}, "Size of allocated memory is invalid");
      }

      var size = interop.asInt(args[1]);
      var targetSize = size * unit.sizeInBytes();
      ByteBuffer buffer = MemoryUtil.directBuffer(interop.asLong(args[0]), targetSize);
      buffer.order(ByteOrder.LITTLE_ENDIAN);
      if (args.length == 3) {
        if (!interop.isNumber(args[2]) || !interop.fitsInLong(args[2])) {
          throw UnsupportedTypeException.create(
              new Object[] {args[2]}, "Address of non-null bitmap is invalid");
        }
        ByteBuffer validityMap =
            MemoryUtil.directBuffer(interop.asLong(args[2]), (int) Math.ceil(size / 8) + 1);
        return new ByteBufferDirect(buffer, validityMap);
      } else {
        return new ByteBufferDirect(buffer, size);
      }
    }

    @Fallback
    static Object doOther(ArrowCastToFixedSizeArrayFactory receiver, Object[] args) {
      throw CompilerDirectives.shouldNotReachHere(unknownLayoutMessage(receiver.getLayout()));
    }

    @CompilerDirectives.TruffleBoundary
    private static String unknownLayoutMessage(LogicalLayout layout) {
      return "unknown layout: " + layout.toString();
    }
  }
}
