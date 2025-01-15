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
import org.enso.interpreter.arrow.LogicalLayout;

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
      var unit = LogicalLayout.Date32;
      var pair = pointer(args, iop, unit);
      return new ArrowFixedArrayDate(pair.buffer(), pair.at(), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Date64")
    static Object doDate64(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = LogicalLayout.Date64;
      var pair = pointer(args, iop, unit);
      return new ArrowFixedArrayDate(pair.buffer(), pair.at(), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int8")
    static Object doInt8(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = LogicalLayout.Int8;
      var pair = pointer(args, iop, unit);
      return new ArrowFixedArrayInt(pair.buffer(), pair.at(), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int16")
    static Object doInt16(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = LogicalLayout.Int16;
      var pair = pointer(args, iop, unit);
      return new ArrowFixedArrayInt(pair.buffer(), pair.at(), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int32")
    static Object doInt32(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = LogicalLayout.Int32;
      var pair = pointer(args, iop, unit);
      return new ArrowFixedArrayInt(pair.buffer(), pair.at(), unit);
    }

    @Specialization(guards = "receiver.getLayout() == Int64")
    static Object doInt64(
        ArrowCastToFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
      var unit = LogicalLayout.Int64;
      var pair = pointer(args, iop, unit);
      return new ArrowFixedArrayInt(pair.buffer(), pair.at(), unit);
    }

    @CompilerDirectives.TruffleBoundary
    private static BufferInt pointer(Object[] args, InteropLibrary interop, SizeInBytes unit)
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

      var capacity = interop.asInt(args[1]);
      if (args.length == 3) {
        if (!interop.isNumber(args[2]) || !interop.fitsInLong(args[2])) {
          throw UnsupportedTypeException.create(
              new Object[] {args[2]}, "Address of non-null bitmap is invalid");
        }
        return new BufferInt(
            ByteBufferDirect.fromAddress(
                interop.asLong(args[0]), interop.asLong(args[2]), capacity, unit),
            capacity);
      } else {
        return new BufferInt(
            ByteBufferDirect.fromAddress(interop.asLong(args[0]), capacity, unit), capacity);
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

  private record BufferInt(ByteBufferDirect buffer, int at) {}
}
