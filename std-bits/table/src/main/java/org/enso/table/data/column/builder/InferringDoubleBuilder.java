package org.enso.table.data.column.builder;

import org.enso.table.data.column.storage.Storage;

import java.math.BigInteger;
import java.util.BitSet;

/**
 * A double builder variant that preserves types and can be retyped to Mixed.
 */
public class InferringDoubleBuilder extends DoubleBuilder {
  InferringDoubleBuilder(BitSet isMissing, long[] doubleData, Object[] rawData, int currentSize) {
    super(isMissing, doubleData, currentSize);
    this.rawData = rawData;
  }

  private Object[] rawData;

  @Override
  public void writeTo(Object[] items) {
    assert rawData != null;
    if (currentSize >= 0) System.arraycopy(rawData, 0, items, 0, currentSize);

    // Since we are retyping to Mixed, the precision loss warnings should not be inherited - thus we reset them.
    precisionLoss = null;
  }

  @Override
  public void appendBulkStorage(Storage<?> storage) {
    throw new UnsupportedOperationException("appendBulkStorage is not supported on InferringDoubleBuilder. " +
        "A DoubleBuilder or MixedBuilder should be used instead. This is a bug in the Table library.");
  }

  @Override
  public void appendDouble(double x) {
    if (currentSize >= this.data.length) {
      grow();
    }

    setRaw(currentSize, x);
    data[currentSize] = Double.doubleToRawLongBits(x);
    currentSize++;
  }

  @Override
  public void appendLong(long integer) {
    if (currentSize >= this.data.length) {
      grow();
    }

    setRaw(currentSize, integer);
    double convertedFloatValue = convertIntegerToDouble(integer);
    data[currentSize] = Double.doubleToRawLongBits(convertedFloatValue);
    currentSize++;
  }

  @Override
  public void appendBigInteger(BigInteger integer) {
    if (currentSize >= this.data.length) {
      grow();
    }

    setRaw(currentSize, integer);
    double convertedFloatValue = convertBigIntegerToDouble(integer);
    data[currentSize] = Double.doubleToRawLongBits(convertedFloatValue);
    currentSize++;
  }

  @Override
  public void appendNoGrow(Object o) {
    setRaw(currentSize, o);
    super.appendNoGrow(o);
  }

  @Override
  public void appendRawNoGrow(long rawData) {
    throw new UnsupportedOperationException("appendRawNoGrow is not supported on InferringDoubleBuilder. " +
        "A DoubleBuilder should be used instead. This is a bug in the Table library.");
  }

  private void setRaw(int ix, Object o) {
    assert rawData != null;
    if (rawData.length <= ix) {
      int newLength = Math.max(rawData.length * 3 / 2 + 1, ix + 1);
      Object[] newRawData = new Object[newLength];
      System.arraycopy(rawData, 0, newRawData, 0, rawData.length);
      rawData = newRawData;
    }

    rawData[ix] = o;
  }
}
