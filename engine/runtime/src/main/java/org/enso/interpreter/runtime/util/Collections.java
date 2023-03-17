package org.enso.interpreter.runtime.util;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.profiles.BranchProfile;
import java.util.Arrays;

public class Collections {

  /** PE-friendly implementation of ArrayList. */
  public static final class ArrayListObj<T> {
    private Object[] data;
    private int size;

    public ArrayListObj(int capacity) {
      this.data = new Object[capacity];
    }

    public ArrayListObj() {
      this(8);
    }

    public void add(T value) {
      add(value, BranchProfile.getUncached());
    }

    public void add(T value, BranchProfile capacityExceededProfile) {
      if (size == data.length) {
        capacityExceededProfile.enter();
        data = Arrays.copyOf(data, size * 2);
      }
      data[size++] = value;
    }

    public void set(int index, T value) {
      checkIndex(index);
      data[index] = value;
    }

    @SuppressWarnings("unchecked")
    public T get(int index) {
      checkIndex(index);
      return (T) data[index];
    }

    @SuppressWarnings("unchecked")
    public T remove(int index) {
      checkIndex(index);
      T result = (T) data[index];
      int lastIdx = size - 1;
      int toMoveLen = lastIdx - index;
      if (toMoveLen > 0) {
        System.arraycopy(data, index + 1, data, index, toMoveLen);
      }
      data[lastIdx] = null;
      size--;
      return result;
    }

    public int size() {
      return size;
    }

    public Object[] toArray() {
      return Arrays.copyOf(data, size);
    }

    public T[] toArray(Class<? extends T[]> newType) {
      return Arrays.copyOf(data, size, newType);
    }

    private void checkIndex(int index) {
      if (!(0 <= index && index < size)) {
        CompilerDirectives.transferToInterpreter();
        throw new IndexOutOfBoundsException(index);
      }
    }
  }
}
