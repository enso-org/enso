package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.InlinedExactClassProfile;
import org.enso.interpreter.arrow.LogicalLayout;

@ExportLibrary(InteropLibrary.class)
public final class ArrowOperationPlus implements TruffleObject {
  private final LogicalLayout layout;

  public ArrowOperationPlus(LogicalLayout layout) {
    this.layout = layout;
  }

  @ExportMessage
  boolean isExecutable() {
    return true;
  }

  static Object args(Object[] args, int index) throws ArityException {
    if (args.length != 2) {
      throw ArityException.create(2, 2, args.length);
    }
    return args[index];
  }

  static Object it(Object[] args, InteropLibrary iop, int index)
      throws ArityException, UnsupportedMessageException {
    if (args.length != 2) {
      throw ArityException.create(2, 2, args.length);
    }
    return iop.getIterator(args[index]);
  }

  @ExportMessage(limit = "3")
  Object execute(
      Object[] args,
      @Bind("$node") Node node,
      @Cached ArrowFixedSizeArrayFactory.InstantiateNode factory,
      @CachedLibrary("args(args, 0)") InteropLibrary iopArray0,
      @CachedLibrary("args(args, 1)") InteropLibrary iopArray1,
      @CachedLibrary("it(args, iopArray0, 0)") InteropLibrary iopIt0,
      @CachedLibrary("it(args, iopArray1, 1)") InteropLibrary iopIt1,
      @CachedLibrary(limit = "3") InteropLibrary iopElem,
      @Cached ArrowFixedSizeArrayBuilder.AppendNode append,
      @Cached ArrowFixedSizeArrayBuilder.BuildNode build,
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
    var it0 = iopArray0.getIterator(arr0);
    var it1 = iopArray1.getIterator(arr1);
    var builder = factory.allocateBuilder(layout, len);

    for (long i = 0; i < len; i++) {
      try {
        var elem0 = iopIt0.getIteratorNextElement(it0);
        var elem1 = iopIt1.getIteratorNextElement(it1);
        long res;
        if (iopElem.fitsInLong(elem0) && iopElem.fitsInLong(elem1)) {
          var l0 = iopElem.asLong(elem0);
          var l1 = iopElem.asLong(elem1);
          try {
            res = Math.addExact(l0, l1);
          } catch (ArithmeticException ex) {
            res = -1;
          }
        } else {
          throw UnsupportedTypeException.create(new Object[] {elem0, elem1});
        }
        append.executeAppend(builder, res);
      } catch (StopIterationException ex) {
        throw UnsupportedTypeException.create(new Object[] {it0, it1});
      }
    }
    return build.executeBuild(builder);
  }
}
