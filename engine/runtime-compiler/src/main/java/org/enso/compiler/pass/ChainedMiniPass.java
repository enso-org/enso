package org.enso.compiler.pass;

import java.util.Objects;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;

/** Utility class for chaining mini passes together. */
final class ChainedMiniPass extends MiniIRPass {
  private MiniIRPass firstPass;
  private MiniIRPass secondPass;

  private ChainedMiniPass(MiniIRPass firstPass, MiniIRPass secondPass) {
    this.firstPass = firstPass;
    this.secondPass = secondPass;
  }

  static MiniIRPass chain(MiniIRPass firstPass, MiniIRPass secondPass) {
    if (firstPass == null) {
      return secondPass;
    }
    return new ChainedMiniPass(firstPass, secondPass);
  }

  @Override
  public MiniIRPass prepare(Expression current) {
    firstPass = firstPass.prepare(current);
    secondPass = secondPass.prepare(current);
    return this;
  }

  @Override
  public Expression transformExpression(Expression ir) {
    var transformedIr = firstPass.transformExpression(ir);
    return secondPass.transformExpression(transformedIr);
  }

  @Override
  public boolean checkPostCondition(IR ir) {
    return firstPass.checkPostCondition(ir) && secondPass.checkPostCondition(ir);
  }

  @Override
  public String toString() {
    return Objects.toString(firstPass) + ":" + Objects.toString(secondPass);
  }
}
