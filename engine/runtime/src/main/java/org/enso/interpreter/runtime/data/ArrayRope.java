package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.Arrays;
import java.util.function.Function;

@ExportLibrary(InteropLibrary.class)
public final class ArrayRope<T> implements EnsoObject {
  private final ArrayRopeSegment<T> segment;

  private ArrayRope(ArrayRopeSegment<T> segment) {
    this.segment = segment;
  }

  @SafeVarargs
  public ArrayRope(T... elements) {
    segment = new ArraySegment<>(elements);
  }

  public ArrayRope<T> append(ArrayRope<T> that) {
    return new ArrayRope<>(new ConcatSegment<>(this.segment, that.segment));
  }

  @SafeVarargs
  public final ArrayRope<T> append(T... items) {
    return new ArrayRope<>(new ConcatSegment<>(this.segment, new ArraySegment<>(items)));
  }

  public ArrayRope<T> prepend(ArrayRope<T> that) {
    return new ArrayRope<>(new ConcatSegment<>(that.segment, this.segment));
  }

  @SafeVarargs
  public final ArrayRope<T> prepend(T... items) {
    return new ArrayRope<>(new ConcatSegment<>(new ArraySegment<>(items), this.segment));
  }

  // TODO: Remove TruffleBoundary?
  @CompilerDirectives.TruffleBoundary
  public T[] toArray(Function<Integer, T[]> genArray) {
    T[] res = genArray.apply(size());
    writeArray(res);
    return res;
  }

  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  long getArraySize() {
    return size();
  }

  @ExportMessage
  @SuppressWarnings("unchecked")
  T readArrayElement(long index) {
    var arr = toArray(size -> (T[]) new Object[size]);
    return arr[Math.toIntExact(index)];
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < size();
  }

  public void writeArray(T[] arr) {
    segment.appendTo(arr, 0);
  }

  public int size() {
    return segment.size();
  }

  @Override
  public String toString() {
    return "ArrayRope{" + "segment=" + segment + '}';
  }

  private interface ArrayRopeSegment<T> {
    void appendTo(T[] builder, int start);

    int size();
  }

  private static final class ArraySegment<T> implements ArrayRopeSegment<T> {
    private final T[] elements;

    public ArraySegment(T[] elements) {
      this.elements = elements;
    }

    @Override
    public void appendTo(T[] builder, int start) {
      System.arraycopy(elements, 0, builder, start, elements.length);
    }

    @Override
    public int size() {
      return elements.length;
    }

    @Override
    public String toString() {
      return "ArraySegment{" + "elements=" + Arrays.toString(elements) + '}';
    }
  }

  private static final class ConcatSegment<T> implements ArrayRopeSegment<T> {
    private final ArrayRopeSegment<T> left;
    private final ArrayRopeSegment<T> right;
    private int cachedSize = UNKNOWN;
    private static final int UNKNOWN = -1;

    public ConcatSegment(ArrayRopeSegment<T> left, ArrayRopeSegment<T> right) {
      this.left = left;
      this.right = right;
    }

    @Override
    public void appendTo(T[] builder, int start) {
      left.appendTo(builder, start);
      right.appendTo(builder, start + left.size());
    }

    @Override
    public int size() {
      if (cachedSize == UNKNOWN) {
        cachedSize = left.size() + right.size();
      }
      return cachedSize;
    }

    @Override
    public String toString() {
      return "ConcatSegment{"
          + "left="
          + left
          + ", right="
          + right
          + ", cachedSize="
          + cachedSize
          + '}';
    }
  }
}
