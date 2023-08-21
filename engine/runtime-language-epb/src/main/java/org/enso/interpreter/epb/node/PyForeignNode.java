package org.enso.interpreter.epb.node;

import java.time.LocalDate;

import org.enso.interpreter.epb.EpbContext;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NodeField;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.source.Source;

@NodeField(name = "foreignFunction", type = Object.class)
public abstract class PyForeignNode extends ForeignFunctionCallNode {

  private @Child CoercePrimitiveNode coercePrimitiveNode = CoercePrimitiveNode.build();

  abstract Object getForeignFunction();

  @CompilerDirectives.TruffleBoundary
  private Object wrapPythonDate(LocalDate date) {

    var ctx = EpbContext.get(this);
    var src = Source.newBuilder("python", """
    from datetime import date

    def conv(y, m, d):
        return date(y, m, d)

    conv
    """, "convert.py").build();

    var conv = ctx.getEnv().parsePublic(src).call();

    try {
      return InteropLibrary.getUncached().execute(conv, date.getYear(), date.getMonthValue(), date.getDayOfMonth());
    } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @Specialization
  public Object doExecute(
      Object[] arguments,
      @CachedLibrary("foreignFunction") InteropLibrary interopLibrary,
      @CachedLibrary(limit="3") InteropLibrary iop
  ) {
    try {
      for (int i = 0; i < arguments.length; i++) {
        if (iop.isDate(arguments[i])) {
          arguments[i] = wrapPythonDate(iop.asDate(arguments[i]));
        }
      }
      return coercePrimitiveNode.execute(interopLibrary.execute(getForeignFunction(), arguments));
    } catch (UnsupportedMessageException | UnsupportedTypeException | ArityException e) {
      throw new IllegalStateException("Python parser returned a malformed object", e);
    }
  }
}
