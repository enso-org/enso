package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.operation.UnaryOperation;

public final class AbsOperation extends NumericUnaryOperation {
  public static final UnaryOperation INSTANCE = new AbsOperation();

  private AbsOperation() {}

  @Override
  public String getName() {
    return "abs";
  }

  @Override
  protected double doDouble(double x) {
    return Math.abs(x);
  }

  @Override
  protected long doLong(long x) {
    return Math.abs(x);
  }

  @Override
  protected BigInteger doBigInteger(BigInteger x) {
    return x.abs();
  }

  @Override
  protected BigDecimal doBigDecimal(BigDecimal x) {
    return x.abs();
  }
}
