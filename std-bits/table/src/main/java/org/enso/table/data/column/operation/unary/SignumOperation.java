package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.type.IntegerType;

public final class SignumOperation extends NumericUnaryTypedOperation<Long> {
  public static final UnaryOperation INSTANCE = new SignumOperation();

  private SignumOperation() {
    super("signum", IntegerType.INT_8);
  }

  @Override
  protected void doLong(BuilderForType<Long> builder, long x) {
    builder.append(Long.signum(x));
  }

  @Override
  protected void doDouble(BuilderForType<Long> builder, double x) {
    builder.append(Math.signum(x));
  }

  @Override
  protected void doBigInteger(BuilderForType<Long> builder, BigInteger x) {
    builder.append(x.signum());
  }

  @Override
  protected void doBigDecimal(BuilderForType<Long> builder, BigDecimal x) {
    builder.append(x.signum());
  }
}
