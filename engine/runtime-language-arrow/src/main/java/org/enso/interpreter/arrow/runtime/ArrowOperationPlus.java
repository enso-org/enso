package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

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

  @ExportMessage
  Object execute(Object[] args, @CachedLibrary(limit = "3") InteropLibrary iop)
      throws ArityException, UnsupportedTypeException, UnsupportedMessageException {
    if (args.length != 2) {
      throw ArityException.create(2, 2, args.length);
    }
    var arr0 = args[0];
    var arr1 = args[1];
    if (!iop.hasArrayElements(arr0) || !iop.hasArrayElements(arr1)) {
      throw UnsupportedTypeException.create(args);
    }
    var len = iop.getArraySize(arr0);
    if (len != iop.getArraySize(arr1)) {
      throw UnsupportedTypeException.create(args, "Arrays must have the same length");
    }
    var builder = iop.instantiate(factory, len);
    try {
      for (long i = 0; i < len; i++) {
        var elem0 = iop.readArrayElement(arr0, i);
        var elem1 = iop.readArrayElement(arr1, i);
        Object res;
        if (iop.isNull(elem0)) {
          res = elem1;
        } else if (iop.isNull(elem1)) {
          res = elem0;
        } else {
          var l0 = iop.asLong(elem0);
          var l1 = iop.asLong(elem1);
          res = l0 + l1;
        }
        iop.invokeMember(builder, "append", res);
      }
      return iop.invokeMember(builder, "build");
    } catch (InvalidArrayIndexException | UnknownIdentifierException ex) {
      throw raise(RuntimeException.class, ex);
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> type, Throwable t) throws E {
    throw (E) t;
  }
}
