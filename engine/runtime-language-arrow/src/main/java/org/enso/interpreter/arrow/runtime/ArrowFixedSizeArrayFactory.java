package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.GenerateUncached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowFixedSizeArrayFactory implements TruffleObject {

  private final LogicalLayout logicalLayout;

  public ArrowFixedSizeArrayFactory(LogicalLayout logicalLayout) {
    this.logicalLayout = logicalLayout;
  }

  @ExportMessage
  boolean isInstantiable() {
    return true;
  }

  public LogicalLayout getLayout() {
    return logicalLayout;
  }

  @ExportMessage
  ArrowFixedSizeArrayBuilder instantiate(
      Object[] args,
      @Cached InstantiateNode instantiate,
      @CachedLibrary(limit = "1") InteropLibrary iop)
      throws UnsupportedMessageException {
    var size = arraySize(args, iop);
    return instantiate.allocateBuilder(logicalLayout, size);
  }

  private static int arraySize(Object[] args, InteropLibrary interop)
      throws UnsupportedMessageException {
    if (args.length != 1 || !interop.isNumber(args[0]) || !interop.fitsInInt(args[0])) {
      throw UnsupportedMessageException.create();
    }
    return interop.asInt(args[0]);
  }

  @GenerateUncached
  abstract static class InstantiateNode extends Node {
    abstract ArrowFixedSizeArrayBuilder executeNew(LogicalLayout logicalLayout, long size);

    @Specialization
    final ArrowFixedSizeArrayBuilder allocateBuilder(LogicalLayout logicalLayout, long size) {
      try {
        return new ArrowFixedSizeArrayBuilder(Math.toIntExact(size), logicalLayout);
      } catch (ArithmeticException ex) {
        CompilerDirectives.transferToInterpreter();
        throw ex;
      }
    }
  }
}
