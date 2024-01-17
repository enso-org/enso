package org.enso.interpreter.node.expression.builtin.number.integer;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Integer", name = "bit_shift_r", description = "Bitwise right-shift.")
public abstract class BitShiftRightNode extends IntegerNode {

  BitShiftRightNode() {
    super("bit_shift_r");
  }

  abstract Object execute(VirtualFrame frame, Object self, Object that);

  static BitShiftRightNode build() {
    return BitShiftRightNodeGen.create();
  }

  @Specialization
  Object doBigInteger(
      VirtualFrame frame,
      long self,
      long that,
      @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(frame, self, -1L * that);
  }

  @Specialization
  Object doBigInteger(
      VirtualFrame frame,
      long self,
      EnsoBigInteger that,
      @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(
        frame, self, new EnsoBigInteger(BigIntegerOps.negate(that.getValue())));
  }

  @Specialization
  Object doBigInteger(
      VirtualFrame frame,
      EnsoBigInteger self,
      long that,
      @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(frame, self, -1L * that);
  }

  @Specialization
  Object doBigInteger(
      VirtualFrame frame,
      EnsoBigInteger self,
      EnsoBigInteger that,
      @Shared("bitShiftNode") @Cached("build()") BitShiftNode bitShiftNode) {
    return bitShiftNode.execute(
        frame, self, new EnsoBigInteger(BigIntegerOps.negate(that.getValue())));
  }

  @Specialization(guards = "isForeignNumber(iop, that)")
  Object doInterop(
      VirtualFrame frame,
      Object self,
      TruffleObject that,
      @CachedLibrary(limit = "3") InteropLibrary iop,
      @Cached BitShiftRightNode delegate) {
    return super.doInterop(frame, self, that, iop, delegate);
  }

  @Fallback
  Object doOther(VirtualFrame frame, Object self, Object that) {
    return super.doOther(frame, self, that);
  }
}
