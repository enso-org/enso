package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.ImportStatic;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.BitSet;
import org.enso.interpreter.arrow.ArrowParser;
import org.enso.interpreter.arrow.util.MemoryUtil;

@ExportLibrary(InteropLibrary.class)
public class ArrowCastToFixedSizeArrayFactory implements TruffleObject {

  private final ArrowParser.LogicalLayout logicalLayout;

  public ArrowCastToFixedSizeArrayFactory(ArrowParser.LogicalLayout logicalLayout) {
    this.logicalLayout = logicalLayout;
  }

  @ExportMessage
  public boolean isInstantiable() {
    return true;
  }

  public ArrowParser.LogicalLayout getLayout() {
    return logicalLayout;
  }

  @ExportMessage
  @ImportStatic(ArrowParser.LogicalLayout.class)
  static class Instantiate {
    @Specialization(guards = "receiver.getLayout() == Date32")
    static Object doDate32(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      var unit = ArrowFixedArrayDate.DateUnit.Day;
      return new ArrowFixedArrayDate(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Date64")
    static Object doDate64(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      var unit = ArrowFixedArrayDate.DateUnit.Millisecond;
      return new ArrowFixedArrayDate(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int8")
    static Object doInt8(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte1;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int16")
    static Object doInt16(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte2;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int32")
    static Object doInt32(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte4;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int64")
    static Object doInt64(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      var unit = ArrowFixedArrayInt.IntUnit.Byte8;
      return new ArrowFixedArrayInt(pointer(args, iop, unit), unit);
    }

    @CompilerDirectives.TruffleBoundary
    private static ByteBufferProxy pointer(Object[] args, InteropLibrary interop, SizeInBytes unit)
        throws UnsupportedMessageException {
      if (args.length < 2
          || !interop.isNumber(args[0])
          || !interop.fitsInLong(args[0])
          || !interop.isNumber(args[1])
          || !interop.fitsInInt(args[1])) {
        throw UnsupportedMessageException.create();
      }
      var size = interop.asInt(args[1]);
      var targetSize = size * unit.sizeInBytes();
      ByteBuffer buffer = MemoryUtil.directBuffer(interop.asLong(args[0]), targetSize);
      buffer.order(ByteOrder.LITTLE_ENDIAN);
      if (args.length == 3) {
        ByteBuffer validityMap =
            MemoryUtil.directBuffer(interop.asLong(args[2]), (int) Math.ceil((size + 7) / 8));
        // TODO avoid copying
        return new ByteBufferDirect(buffer, BitSet.valueOf(validityMap));
      } else {
        return new ByteBufferDirect(buffer, size);
      }
    }

    @Fallback
    static Object doOther(ArrowCastToFixedSizeArrayFactory receiver, Object[] args) {
      throw CompilerDirectives.shouldNotReachHere(unknownLayoutMessage(receiver.getLayout()));
    }

    @CompilerDirectives.TruffleBoundary
    private static String unknownLayoutMessage(ArrowParser.LogicalLayout layout) {
      return "unknown layout: " + layout.toString();
    }
  }
}
