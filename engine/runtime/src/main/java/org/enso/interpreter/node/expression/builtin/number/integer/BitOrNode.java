package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "bit_or", description = "Bitwise or.")
public abstract class BitOrNode extends IntegerNode {
  BitOrNode() {
    super("bit_or");
  }

  abstract Object execute(VirtualFrame frame, Object own, Object that);

  static BitOrNode build() {
    return BitOrNodeGen.create();
  }

  @Specialization
  long doLong(long self, long that) {
    return self | that;
  }

  @Specialization
  Object doBigInteger(long self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitOr(self, that.getValue()));
  }

  @Specialization
  Object doLong(EnsoBigInteger self, long that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitOr(self.getValue(), that));
  }

  @Specialization
  Object doBigInteger(EnsoBigInteger self, EnsoBigInteger that) {
    return toEnsoNumberNode.execute(BigIntegerOps.bitOr(self.getValue(), that.getValue()));
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  Object doInterop(
      VirtualFrame frame,
      Object self,
      TruffleObject that,
      @CachedLibrary(limit = "3") InteropLibrary iop,
      @Cached BitOrNode delegate) {
    return super.doInterop(frame, self, that, iop, delegate);
  }

  @Fallback
  Object doOther(VirtualFrame frame, Object self, Object that) {
    return super.doOther(frame, self, that);
  }
}
