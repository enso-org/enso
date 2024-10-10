package org.enso.compiler.pass;

import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;

/** Utility class for chaining mini passes together. */
public final class MiniPassChainer {
  private MiniPassChainer() {}

  /**
   * Chains a list of mini passes into a single pass that applies them in order.
   *
   * @param passes Ordered list of mini passes.
   * @return Chained mini pass that applies all the provided mini passes in the given order.
   */
  public static MiniIRPass chain(List<MiniIRPass> passes) {
    if (passes.size() < 2) {
      throw new IllegalArgumentException("At least two mini passes are required to chain them");
    }
    MiniIRPass chainedPass = null;
    for (int i = 0; i < passes.size() - 1; i++) {
      var currPass = passes.get(i);
      var nextPass = passes.get(i + 1);
      chainedPass = chain(currPass, nextPass);
    }
    return chainedPass;
  }

  public static MiniIRPass chain(MiniIRPass firstPass, MiniIRPass secondPass) {
    return new ChainedMiniPass(firstPass, secondPass);
  }
}

class ChainedMiniPass extends MiniIRPass {
  private MiniIRPass firstPass;
  private MiniIRPass secondPass;

  ChainedMiniPass(MiniIRPass firstPass, MiniIRPass secondPass) {
    this.firstPass = firstPass;
    this.secondPass = secondPass;
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
}
