package org.enso.interpreter.node.expression.builtin.number.decimal;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.ConditionProfile;
import java.math.BigDecimal;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.number.utils.BigIntegerOps;
import org.enso.interpreter.runtime.number.EnsoBigInteger;

@BuiltinMethod(type = "Decimal", name = "floor", description = "Decimal floor.")
public class FloorNode extends Node {
  private final ConditionProfile fitsProfile = ConditionProfile.createCountingProfile();

  /**
   * Execute the node.
   *
   * @param _this the double to calculate the floor of
   * @return a long with the floor value, if possible, otherwise a {@link EnsoBigInteger} otherwise
   */
  public Object execute(double _this) {
    double floor = Math.floor(_this);
    if (fitsProfile.profile(BigIntegerOps.fitsInLong(floor))) {
      return (long) floor;
    } else {
      return new EnsoBigInteger(BigDecimal.valueOf(floor).toBigIntegerExact());
    }
  }
}
