package org.enso.table.data.column.builder.object;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Objects;

import org.enso.base.polyglot.NumericConverter;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.DoubleStorage;
import org.enso.table.data.column.storage.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.util.BitSets;

/**
 * A builder for numeric columns.
 */
public class NumericBuilder extends TypedBuilder {
    private final BitSet isMissing = new BitSet();
    private long[] data;
    private boolean isDouble;
    private int currentSize;

    private NumericBuilder(boolean isDouble, int size) {
        this.data = new long[size];
        this.isDouble = isDouble;
    }

    public static NumericBuilder createDoubleBuilder(int size) {
        return new NumericBuilder(true, size);
    }

    public static NumericBuilder createLongBuilder(int size) {
        return new NumericBuilder(false, size);
    }

    @Override
    public void writeTo(Object[] items) {
        for (int i = 0; i < currentSize; i++) {
            if (isMissing.get(i)) {
                items[i] = null;
            } else if (isDouble) {
                items[i] = Double.longBitsToDouble(data[i]);
            } else {
                items[i] = data[i];
            }
        }
    }

    @Override
    public boolean canRetypeTo(StorageType type) {
        return !this.isDouble && Objects.equals(type, FloatType.FLOAT_64);
    }

    @Override
    public TypedBuilder retypeTo(StorageType type) {
        if (!this.isDouble && Objects.equals(type, FloatType.FLOAT_64)) {
            this.isDouble = true;
            for (int i = 0; i < currentSize; i++) {
                data[i] = Double.doubleToRawLongBits((double) data[i]);
            }
            return this;
        } else {
            throw new UnsupportedOperationException();
        }
    }

    @Override
    public StorageType getType() {
        return isDouble ? FloatType.FLOAT_64 : IntegerType.INT_64;
    }

    @Override
    public void appendNoGrow(Object o) {
        if (o == null) {
            isMissing.set(currentSize++);
        } else if (isDouble) {
            double value = NumericConverter.coerceToDouble(o);
            data[currentSize++] = Double.doubleToRawLongBits(value);
        } else {
            data[currentSize++] = NumericConverter.coerceToLong(o);
        }
    }

    @Override
    public boolean accepts(Object o) {
        if (isDouble) {
            return NumericConverter.isCoercibleToDouble(o);
        } else {
            return NumericConverter.isCoercibleToLong(o);
        }
    }

    @Override
    public void append(Object o) {
        if (currentSize >= data.length) {
            grow();
        }
        appendNoGrow(o);
    }

    @Override
    public void appendNulls(int count) {
        isMissing.set(currentSize, currentSize + count);
        currentSize += count;
    }

    @Override
    public void appendBulkStorage(Storage<?> storage) {
        if (isDouble) {
            appendBulkDouble(storage);
        } else {
            appendBulkLong(storage);
        }
    }

    private void ensureFreeSpaceFor(int additionalSize) {
        if (currentSize + additionalSize > data.length) {
            grow(currentSize + additionalSize);
        }
    }

    private void appendBulkDouble(Storage<?> storage) {
        if (Objects.equals(storage.getType(), FloatType.FLOAT_64)) {
            if (storage instanceof DoubleStorage doubleStorage) {
                int n = doubleStorage.size();
                ensureFreeSpaceFor(n);
                System.arraycopy(doubleStorage.getRawData(), 0, data, currentSize, n);
                BitSets.copy(doubleStorage.getIsMissing(), isMissing, currentSize, n);
                currentSize += n;
            } else {
                throw new IllegalStateException(
                        "Unexpected storage implementation for type DOUBLE: "
                                + storage
                                + ". This is a bug in the Table library.");
            }
        } else if (Objects.equals(storage.getType(), IntegerType.INT_64)) {
            if (storage instanceof LongStorage longStorage) {
                int n = longStorage.size();
                BitSets.copy(longStorage.getIsMissing(), isMissing, currentSize, n);
                for (int i = 0; i < n; i++) {
                    data[currentSize++] = Double.doubleToRawLongBits((double) longStorage.getItem(i));
                }
            } else {
                throw new IllegalStateException(
                        "Unexpected storage implementation for type LONG: "
                                + storage
                                + ". This is a bug in the Table library.");
            }
        } else if (Objects.equals(storage.getType(), BooleanType.INSTANCE)) {
            if (storage instanceof BoolStorage boolStorage) {
                int n = boolStorage.size();
                for (int i = 0; i < n; i++) {
                    if (boolStorage.isNa(i)) {
                        isMissing.set(currentSize++);
                    } else {
                        double x = booleanAsDouble(boolStorage.getItem(i));
                        data[currentSize++] = Double.doubleToRawLongBits(x);
                    }
                }
            } else {
                throw new IllegalStateException(
                        "Unexpected storage implementation for type BOOLEAN: "
                                + storage
                                + ". This is a bug in the Table library.");
            }
        } else {
            throw new StorageTypeMismatch(getType(), storage.getType());
        }
    }

    private void appendBulkLong(Storage<?> storage) {
        if (Objects.equals(storage.getType(), IntegerType.INT_64)) {
            if (storage instanceof LongStorage longStorage) {
                int n = longStorage.size();
                ensureFreeSpaceFor(n);
                System.arraycopy(longStorage.getRawData(), 0, data, currentSize, n);
                BitSets.copy(longStorage.getIsMissing(), isMissing, currentSize, n);
                currentSize += n;
            } else {
                throw new IllegalStateException(
                        "Unexpected storage implementation for type DOUBLE: "
                                + storage
                                + ". This is a bug in the Table library.");
            }
        } else if (Objects.equals(storage.getType(), BooleanType.INSTANCE)) {
            if (storage instanceof BoolStorage boolStorage) {
                int n = boolStorage.size();
                for (int i = 0; i < n; i++) {
                    if (boolStorage.isNa(i)) {
                        isMissing.set(currentSize++);
                    } else {
                        data[currentSize++] = booleanAsLong(boolStorage.getItem(i));
                    }
                }
            } else {
                throw new IllegalStateException(
                        "Unexpected storage implementation for type BOOLEAN: "
                                + storage
                                + ". This is a bug in the Table library.");
            }
        } else {
            throw new StorageTypeMismatch(getType(), storage.getType());
        }
    }

    private long booleanAsLong(boolean value) {
        return value ? 1L : 0L;
    }

    private double booleanAsDouble(boolean value) {
        return value ? 1.0 : 0.0;
    }

    /**
     * Append a new item in raw form to this builder, assuming that it has enough allocated space.
     *
     * <p>This function should only be used when it is guaranteed that the builder has enough
     * capacity, for example if it was initialized with an initial capacity known up-front.
     *
     * @param rawData the raw encoding of the item, for long numbers just the number and for doubles,
     *                its long bytes
     */
    public void appendRawNoGrow(long rawData) {
        data[currentSize++] = rawData;
    }

    /**
     * Append a new integer to this builder.
     *
     * @param data the integer to append
     */
    public void appendLong(long data) {
        int wasSize = currentSize;
        int wasLength = this.data.length;

        if (currentSize >= this.data.length) {
            grow();
        }

        if (currentSize >= this.data.length) {
            throw new IllegalStateException("currentSize=" + currentSize + "; wasSize=" + wasSize + "; wasLength=" + wasLength + "; data.length=" + this.data.length);
        }
        appendRawNoGrow(data);
    }

    /**
     * Append a new double to this builder.
     *
     * @param data the double to append
     */
    public void appendDouble(double data) {
        if (currentSize >= this.data.length) {
            grow();
        }
        appendRawNoGrow(Double.doubleToRawLongBits(data));
    }

    @Override
    public int getCurrentSize() {
        return currentSize;
    }

    @Override
    public Storage<?> seal() {
        if (isDouble) {
            return new DoubleStorage(data, currentSize, isMissing);
        } else {
            return new LongStorage(data, currentSize, isMissing);
        }
    }

    /**
     * Grows the underlying array.
     * <p>
     * The method grows the array by 50% by default to amortize the re-allocation time over appends.
     * It tries to keep the invariant that after calling `grow` the array has at least one free slot.
     */
    private void grow() {
        int desiredCapacity = 3;
        if (data.length > 1) {
            desiredCapacity = (data.length * 3 / 2);
        }

        // It is possible for the `currentSize` to grow arbitrarily larger than
        // the capacity, because when nulls are being added the array is not
        // resized, only the counter is incremented. Thus, we need to ensure
        // that we have allocated enough space for at least one element.
        if (currentSize >= desiredCapacity) {
            desiredCapacity = currentSize + 1;
        }

        grow(desiredCapacity);
    }

    private void grow(int desiredCapacity) {
        this.data = Arrays.copyOf(data, desiredCapacity);
    }
}
