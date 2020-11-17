package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigDecimal;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "ceil", description = "Decimal ceiling.")
public class CeilNode extends Node {
  private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

  /**
   * Execute the node.
   *
   * @param _this the double to calculate the ceil of
   * @return a long with the ceil value, if possible, otherwise a {@link EnsoBigInteger} otherwise
   */
  public Object execute(double _this) {
    double ceil = Math.ceil(_this);
    if (fitsProfile.profile(BigIntegerOps.fitsInLong(ceil))) {
      return (long) ceil;
    } else {
      return new EnsoBigInteger(BigDecimal.valueOf(ceil).toBigIntegerExact());
    }
  }
}
