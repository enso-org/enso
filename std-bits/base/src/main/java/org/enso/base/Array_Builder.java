package org.enso.base;

import java.util.Arrays;
import java.util.List;

public class Array_Builder<T> {
  private static final Object[] EMPTY_ARRAY = new Object[0];
  private final int capacity;
  private int size;
  private Object primitiveArray;
  private Object[] objectArray;

  private Array_Builder(int capacity) {
    this.capacity = Math.max(1, capacity);
  }

  /**
   * This function forces the polyglot conversion of an Enso array into a `byte[]`. This allows for
   * asserting that it is a valid `byte[]`.
   *
   * @param input the converted array.
   * @return the `input` unchanged.
   */
  public static byte[] ensureByteArray(byte[] input) {
    return input;
  }

  /** Creates new builder */
  public static <V> Array_Builder<V> newBuilder(int capacity) {
    return new Array_Builder<>(capacity);
  }

  /** Is the builder empty? */
  public boolean isEmpty() {
    return size == 0;
  }

  /**
   * Adds an element to the builder
   *
   * @param e the element to add
   */
  public void add(T e) {
    if (objectArray != null) {
        if (size == objectArray.length) {
            objectArray = Arrays.copyOf(objectArray, size * 2);
        }
        objectArray[size++] = e;
    } else if (primitiveArray instanceof long[] longArray) {
        if (e instanceof Long l) {
            if (size == longArray.length) {
                primitiveArray = longArray = Arrays.copyOf(longArray, size * 2);
            }
            longArray[size++] = l;
        } else {
            objectArray = new Object[longArray.length];
            for (int i = 0; i < size; i++) {
                objectArray[i] = longArray[i];
            }
            primitiveArray = null;
            add(e);
        }
    } else if (primitiveArray instanceof double[] doubleArray) {
        if (e instanceof Double d) {
            if (size == doubleArray.length) {
                primitiveArray = doubleArray = Arrays.copyOf(doubleArray, size * 2);
            }
            doubleArray[size++] = d;
        } else {
            objectArray = new Object[doubleArray.length];
            for (int i = 0; i < size; i++) {
                objectArray[i] = doubleArray[i];
            }
            primitiveArray = null;
            add(e);
        }
    } else {
        assert objectArray == null;
        assert primitiveArray == null;
        assert size == 0;
        if (e instanceof Long l) {
          var arr = new long[capacity];
          arr[0] = l;
          primitiveArray = arr;
        } else if (e instanceof Double d) {
          var arr = new double[capacity];
          arr[0] = d;
          primitiveArray = arr;
        } else {
          var arr = new Object[capacity];
          arr[0] = e;
          objectArray = arr;
        }
        size = 1;
    }
  }

  /** Obtains an element from the builder */
  public Object get(int index) {
    if (objectArray != null) {
        return objectArray[index];
    } else if (primitiveArray instanceof long[] longArray) {
        return longArray[index];
    } else if (primitiveArray instanceof double[] doubleArray) {
        return doubleArray[index];
    } else {
        throw new ArrayIndexOutOfBoundsException();
    }
  }

  /** A temporary workaround to be able to efficiently append an array to `ArrayList`. */
  public void appendTo(List<T> list) {
    for (T obj : list) {
        add(obj);
    }
  }

  /** Returns the current array of the builder. */
  public Object toArray() {
    if (objectArray != null) {
        return objectArray.length == size ? objectArray : Arrays.copyOf(objectArray, size);
    } else if (primitiveArray instanceof long[] longArray) {
        return longArray.length == size ? longArray : Arrays.copyOf(longArray, size);
    } else if (primitiveArray instanceof double[] doubleArray) {
        return doubleArray.length == size ? doubleArray : Arrays.copyOf(doubleArray, size);
    } else {
        return EMPTY_ARRAY;
    }
  }
}
