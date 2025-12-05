package org.enso.table.data.column.operation.unary;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.DoubleUnaryOperator;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.type.FloatType;

public final class DoubleUnaryOperation extends NumericUnaryTypedOperation<Double> {
  public static UnaryOperation SIN_INSTANCE = new DoubleUnaryOperation("sin", Math::sin);
  public static UnaryOperation COS_INSTANCE = new DoubleUnaryOperation("cos", Math::cos);
  public static UnaryOperation TAN_INSTANCE = new DoubleUnaryOperation("tan", Math::tan);
  public static UnaryOperation SINH_INSTANCE = new DoubleUnaryOperation("sinh", Math::sinh);
  public static UnaryOperation COSH_INSTANCE = new DoubleUnaryOperation("cosh", Math::cosh);
  public static UnaryOperation TANH_INSTANCE = new DoubleUnaryOperation("tanh", Math::tanh);
  public static UnaryOperation ASIN_INSTANCE = new DoubleUnaryOperation("asin", Math::asin);
  public static UnaryOperation ACOS_INSTANCE = new DoubleUnaryOperation("acos", Math::acos);
  public static UnaryOperation ATAN_INSTANCE = new DoubleUnaryOperation("atan", Math::atan);
  public static UnaryOperation SQRT_INSTANCE = new DoubleUnaryOperation("sqrt", Math::sqrt);
  public static UnaryOperation LN_INSTANCE = new DoubleUnaryOperation("ln", Math::log);
  public static UnaryOperation EXP_INSTANCE = new DoubleUnaryOperation("exp", Math::exp);
  public static UnaryOperation LOG10_INSTANCE = new DoubleUnaryOperation("log10", Math::log10);

  private final DoubleUnaryOperator function;

  private DoubleUnaryOperation(String name, DoubleUnaryOperator function) {
    super(name, FloatType.FLOAT_64);
    this.function = function;
  }

  @Override
  protected void doLong(BuilderForType<Double> builder, long x) {
    builder.append(function.applyAsDouble(x));
  }

  @Override
  protected void doDouble(BuilderForType<Double> builder, double x) {
    builder.append(function.applyAsDouble(x));
  }

  @Override
  protected void doBigInteger(BuilderForType<Double> builder, BigInteger x) {
    builder.append(function.applyAsDouble(x.doubleValue()));
  }

  @Override
  protected void doBigDecimal(BuilderForType<Double> builder, BigDecimal x) {
    builder.append(function.applyAsDouble(x.doubleValue()));
  }
}
