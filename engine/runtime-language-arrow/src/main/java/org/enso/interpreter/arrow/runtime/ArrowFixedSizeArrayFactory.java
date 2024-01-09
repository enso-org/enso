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
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public class ArrowFixedSizeArrayFactory implements TruffleObject {

  private final LogicalLayout logicalLayout;

  public ArrowFixedSizeArrayFactory(LogicalLayout logicalLayout) {
    this.logicalLayout = logicalLayout;
  }

  @ExportMessage
  public boolean isInstantiable() {
    return true;
  }

  public LogicalLayout getLayout() {
    return logicalLayout;
  }

  @ExportMessage
  @ImportStatic(LogicalLayout.class)
  static class Instantiate {
    @Specialization(guards = "receiver.getLayout() == Date32")
    static Object doDate32(
        ArrowFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      return new ArrowFixedArrayDate(arraySize(args, iop), ArrowFixedArrayDate.DateUnit.Day);
    }

    @Specialization(guards = "receiver.getLayout() == Date64")
    static Object doDate64(
        ArrowFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      return new ArrowFixedArrayDate(
          arraySize(args, iop), ArrowFixedArrayDate.DateUnit.Millisecond);
    }

    @Specialization(guards = "receiver.getLayout() == Int8")
    static Object doInt8(
        ArrowFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      return new ArrowFixedArrayInt(arraySize(args, iop), ArrowFixedArrayInt.IntUnit.Byte1);
    }

    @Specialization(guards = "receiver.getLayout() == Int16")
    static Object doInt16(
        ArrowFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      return new ArrowFixedArrayInt(arraySize(args, iop), ArrowFixedArrayInt.IntUnit.Byte2);
    }

    @Specialization(guards = "receiver.getLayout() == Int32")
    static Object doInt32(
        ArrowFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      return new ArrowFixedArrayInt(arraySize(args, iop), ArrowFixedArrayInt.IntUnit.Byte4);
    }

    @Specialization(guards = "receiver.getLayout() == Int64")
    static Object doInt64(
        ArrowFixedSizeArrayFactory receiver,
        Object[] args,
        @Cached.Shared("interop") @CachedLibrary(limit = "1") InteropLibrary iop)
        throws UnsupportedMessageException {
      return new ArrowFixedArrayInt(arraySize(args, iop), ArrowFixedArrayInt.IntUnit.Byte8);
    }

    @CompilerDirectives.TruffleBoundary
    private static int arraySize(Object[] args, InteropLibrary interop)
        throws UnsupportedMessageException {
      if (args.length != 1 || !interop.isNumber(args[0]) || !interop.fitsInInt(args[0])) {
        throw UnsupportedMessageException.create();
      }
      return interop.asInt(args[0]);
    }

    @Fallback
    static Object doOther(ArrowFixedSizeArrayFactory receiver, Object[] args) {
      throw CompilerDirectives.shouldNotReachHere(unknownLayoutMessage(receiver.getLayout()));
    }

    @CompilerDirectives.TruffleBoundary
    private static String unknownLayoutMessage(LogicalLayout layout) {
      return "unknown layout: " + layout.toString();
    }
  }
}
