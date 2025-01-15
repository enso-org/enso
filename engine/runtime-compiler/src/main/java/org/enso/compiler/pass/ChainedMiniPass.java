package org.enso.compiler.pass;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

/** Utility class for chaining mini passes together. */
final class ChainedMiniPass extends MiniIRPass {
  private final MiniIRPass firstPass;
  private final MiniIRPass secondPass;

  private ChainedMiniPass(MiniIRPass firstPass, MiniIRPass secondPass) {
    this.firstPass = firstPass;
    this.secondPass = secondPass;
  }

  static MiniIRPass chain(MiniIRPass firstPass, MiniIRPass secondPass) {
    if (firstPass == null) {
      return secondPass;
    }
    if (secondPass == null) {
      return firstPass;
    }
    return new ChainedMiniPass(firstPass, secondPass);
  }

  @Override
  public MiniIRPass prepare(IR parent, Expression current) {
    var firstPrepared = firstPass == null ? null : firstPass.prepare(parent, current);
    var secondPrepared = secondPass == null ? null : secondPass.prepare(parent, current);
    if (firstPrepared == firstPass && secondPrepared == secondPass) {
      return this;
    } else {
      return chain(firstPrepared, secondPrepared);
    }
  }

  @Override
  public Expression transformExpression(Expression ir) {
    var fstIr = firstPass == null ? ir : firstPass.transformExpression(ir);
    var sndIr = secondPass == null ? fstIr : secondPass.transformExpression(fstIr);
    return sndIr;
  }

  @Override
  public Module transformModule(Module moduleIr) {
    var firstIr = firstPass == null ? moduleIr : firstPass.transformModule(moduleIr);
    var secondIr = secondPass == null ? firstIr : secondPass.transformModule(firstIr);
    return secondIr;
  }

  @Override
  public boolean checkPostCondition(IR ir) {
    var firstCheck = firstPass == null || firstPass.checkPostCondition(ir);
    var secondCheck = secondPass == null || secondPass.checkPostCondition(ir);
    return firstCheck && secondCheck;
  }

  @Override
  public String toString() {
    return "{" + firstPass + " + " + secondPass + "}";
  }
}
