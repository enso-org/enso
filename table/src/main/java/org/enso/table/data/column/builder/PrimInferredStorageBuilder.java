package org.enso.table.data.column.builder;

import org.enso.table.data.column.DoubleStorage;
import org.enso.table.data.column.LongStorage;
import org.enso.table.data.column.Storage;

import java.util.BitSet;

/**
 * A column builder for numeric types. Tries to interpret all data as 64-bit integers. If that
 * becomes impossible, retypes itself to store 64-bit floats. When even that fails, falls back to a
 * {@link StringStorageBuilder}.
 */
public class PrimInferredStorageBuilder extends StorageBuilder {
  private enum Type {
    LONG,
    DOUBLE
  }

  private int size = 0;
  private long[] data = new long[64];
  private String[] rawData = new String[64];
  private final BitSet isMissing = new BitSet();
  private Type type = Type.LONG;

  /** @inheritDoc */
  @Override
  public StorageBuilder parseAndAppend(String value) {
    if (value == null) {
      ensureAppendable();
      isMissing.set(size);
      size++;
      return this;
    }
    switch (type) {
      case LONG:
        return appendLong(value);
      case DOUBLE:
        return appendDouble(value);
      default:
        throw new IllegalStateException();
    }
  }

  private StorageBuilder appendLong(String value) {
    try {
      long l = Long.parseLong(value);
      ensureAppendable();
      rawData[size] = value;
      data[size] = l;
      size++;
      return this;
    } catch (NumberFormatException ignored) {
      return failedLong(value);
    }
  }

  private StorageBuilder appendDouble(String value) {
    try {
      double d = Double.parseDouble(value);
      ensureAppendable();
      data[size] = Double.doubleToRawLongBits(d);
      rawData[size] = value;
      size++;
      return this;
    } catch (NumberFormatException ignored) {
      return failedDouble(value);
    }
  }

  private StorageBuilder failedLong(String value) {
    try {
      double d = Double.parseDouble(value);
      retypeToDouble();
      ensureAppendable();
      data[size] = Double.doubleToRawLongBits(d);
      rawData[size] = value;
      size++;
      return this;
    } catch (NumberFormatException ignored) {
      return failedDouble(value);
    }
  }

  private StorageBuilder failedDouble(String value) {
    StringStorageBuilder newBuilder = new StringStorageBuilder(rawData, size);
    newBuilder.parseAndAppend(value);
    return newBuilder;
  }

  private void retypeToDouble() {
    for (int i = 0; i < size; i++) {
      data[i] = Double.doubleToRawLongBits(data[i]);
    }
    type = Type.DOUBLE;
  }

  // TODO[MK] Consider storing data `rawData` in non-linear storage to avoid reallocations.
  private void ensureAppendable() {
    if (size >= data.length) {
      long[] newData = new long[2 * data.length];
      String[] newRawData = new String[2 * data.length];
      System.arraycopy(data, 0, newData, 0, data.length);
      System.arraycopy(rawData, 0, newRawData, 0, rawData.length);
      data = newData;
      rawData = newRawData;
    }
  }

  /** @inheritDoc */
  @Override
  public Storage seal() {
    if (type == Type.LONG) {
      return new LongStorage(data, size, isMissing);
    } else {
      return new DoubleStorage(data, size, isMissing);
    }
  }
}
