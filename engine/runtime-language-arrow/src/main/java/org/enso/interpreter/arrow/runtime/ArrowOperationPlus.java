package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.InlinedExactClassProfile;

@ExportLibrary(InteropLibrary.class)
public final class ArrowOperationPlus implements TruffleObject {
  private final ArrowFixedSizeArrayFactory factory;

  public ArrowOperationPlus(ArrowFixedSizeArrayFactory factory) {
    this.factory = factory;
  }

  @ExportMessage
  boolean isExecutable() {
    return true;
  }

  static ArrowFixedSizeArrayFactory factory(ArrowOperationPlus thiz) {
    return thiz.factory;
  }

  static Object args(Object[] args, int index) throws ArityException {
    if (args.length != 2) {
      throw ArityException.create(2, 2, args.length);
    }
    return args[index];
  }

  @ExportMessage(limit = "3")
  Object execute(
      Object[] args,
      @Bind("$node") Node node,
      @CachedLibrary("factory(this)") InteropLibrary iop,
      @CachedLibrary("args(args, 0)") InteropLibrary iopArray0,
      @CachedLibrary("args(args, 1)") InteropLibrary iopArray1,
      @CachedLibrary(limit = "3") InteropLibrary iopElem,
      @CachedLibrary(limit = "3") InteropLibrary iopBuilder,
      @Cached InlinedExactClassProfile typeOfBuf0,
      @Cached InlinedExactClassProfile typeOfBuf1)
      throws ArityException, UnsupportedTypeException, UnsupportedMessageException {
    var arr0 = args[0];
    var arr1 = args[1];
    if (!iopArray0.hasArrayElements(arr0) || !iopArray1.hasArrayElements(arr1)) {
      throw UnsupportedTypeException.create(args);
    }
    var len = iopArray0.getArraySize(arr0);
    if (len != iopArray1.getArraySize(arr1)) {
      throw UnsupportedTypeException.create(args, "Arrays must have the same length");
    }
    var builder = iop.instantiate(factory, len);
    try {
      var buf0 = typeOfBuf0.profile(node, ((ArrowFixedArrayInt) arr0).buffer.dataBuffer);
      var buf1 = typeOfBuf1.profile(node, ((ArrowFixedArrayInt) arr1).buffer.dataBuffer);
      buf0.rewind();
      buf1.rewind();

      for (long i = 0; i < len; i++) {
        var l0 = buf0.getLong();
        var l1 = buf1.getLong();
        var res = l0 + l1;
        iopBuilder.invokeMember(builder, "append", res);
      }
      return iopBuilder.invokeMember(builder, "build");
    } catch (UnknownIdentifierException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> type, Throwable t) throws E {
    throw (E) t;
  }
}
