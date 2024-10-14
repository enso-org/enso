package org.enso.compiler.pass;

import java.util.Objects;
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
    return new ChainedMiniPass(firstPass, secondPass);
  }

  @Override
  public MiniIRPass prepare(Expression current) {
    var first = firstPass.prepare(current);
    var second = secondPass.prepare(current);
    return meOrNew(first, second);
  }

  @Override
  public MiniIRPass prepareForModule(Module current) {
    var first = firstPass.prepareForModule(current);
    var second = secondPass.prepareForModule(current);
    return meOrNew(first, second);
  }

  private ChainedMiniPass meOrNew(MiniIRPass first, MiniIRPass second) {
    if (first == firstPass && second == secondPass) {
      return this;
    } else {
      return new ChainedMiniPass(first, second);
    }
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
