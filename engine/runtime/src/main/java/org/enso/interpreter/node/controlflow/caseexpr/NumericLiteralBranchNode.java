package org.enso.interpreter.node.controlflow.caseexpr;

import java.math.BigInteger;

import org.enso.interpreter.runtime.number.EnsoBigInteger;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.CountingConditionProfile;

@NodeInfo(shortName = "NumericLiteralMatch", description = "Allows matching on numeric literals")
public abstract class NumericLiteralBranchNode extends BranchNode {
  private final Object literal;

  private final CountingConditionProfile numProfile = CountingConditionProfile.create();

  NumericLiteralBranchNode(Object literal, RootCallTarget branch, boolean terminalBranch) {
    super(branch, terminalBranch);
    this.literal = literal;
  }

  public static NumericLiteralBranchNode build(
      long literal, RootCallTarget branch, boolean terminalBranch) {
    return NumericLiteralBranchNodeGen.create(literal, branch, terminalBranch);
  }

  public static NumericLiteralBranchNode build(
      double literal, RootCallTarget branch, boolean terminalBranch) {
    return NumericLiteralBranchNodeGen.create(literal, branch, terminalBranch);
  }

  public static NumericLiteralBranchNode build(
      BigInteger literal, RootCallTarget branch, boolean terminalBranch) {
    return NumericLiteralBranchNodeGen.create(literal, branch, terminalBranch);
  }

  @Specialization(guards = "interop.isNumber(target)")
  void doObject(
      VirtualFrame frame,
      Object state,
      Object target,
      @CachedLibrary(limit = "1") InteropLibrary interop) {
    var taken = switch (literal) {
      case Long l -> target instanceof Long t && l.longValue() == t.longValue();
      case Double d -> target instanceof Double t && d.doubleValue() == t.doubleValue();
      case BigInteger b -> target instanceof EnsoBigInteger e && compare(b, e.asBigInteger());
      default -> throw CompilerDirectives.shouldNotReachHere();
    };
    if (numProfile.profile(taken)) accept(frame, state, new Object[0]);
  }

  @Fallback
  void doOther(VirtualFrame frame, Object state, Object target) {}

  @CompilerDirectives.TruffleBoundary(allowInlining=true)
  private boolean compare(BigInteger b1, BigInteger b2) {
    return b1.equals(b2);
  }
}
