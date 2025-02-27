package org.enso.table.data.column.operation.unary;

import org.enso.table.data.column.operation.UnaryOperation;

import java.math.BigDecimal;
import java.math.BigInteger;

public class AbsOperation extends NumericUnaryOperation {
  public static final UnaryOperation INSTANCE = new AbsOperation();

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
