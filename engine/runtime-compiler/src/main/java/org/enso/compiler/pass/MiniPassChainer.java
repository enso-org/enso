package org.enso.compiler.pass;

import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.pass.IRPass.IRMetadata;

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
    return new MiniIRPass() {
      @Override
      public MiniIRPass prepare(IR ir) {
        throw new UnsupportedOperationException("unimplemented");
      }

      @Override
      public IR transformIr(IR ir) {
        IR transformedIr = ir;
        for (var miniPass : passes) {
          transformedIr = miniPass.transformIr(transformedIr);
        }
        return transformedIr;
      }

      @Override
      public boolean checkPostCondition(IR ir) {
        return passes.stream().allMatch(pass -> pass.checkPostCondition(ir));
      }
    };
  }

  public static <T extends IRMetadata> MiniIRPass chain(
      MiniIRPass firstPass, MiniIRPass secondPass) {
    return new MiniIRPass() {
      @Override
      public MiniIRPass prepare(IR ir) {
        throw new UnsupportedOperationException("unimplemented");
      }

      @Override
      public IR transformIr(IR ir) {
        IR firstResult = firstPass.transformIr(ir);
        return secondPass.transformIr(firstResult);
      }

      @Override
      public boolean checkPostCondition(IR ir) {
        return firstPass.checkPostCondition(ir) && secondPass.checkPostCondition(ir);
      }
    };
  }
}
