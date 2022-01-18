package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.library.dispatch.MethodDispatchLibrary;

@ExportLibrary(MethodDispatchLibrary.class)
public class WithWarnings implements TruffleObject {
  private final ArrayRope<Object> warnings;
  private final Object value;

  private static final class ArrayRope<T> {
    private ArrayRopeSegment<T> segment;

    private ArrayRope(ArrayRopeSegment<T> segment) {
      this.segment = segment;
    }

    public ArrayRope(T... elements) {
      segment = new ArraySegment<>(elements);
    }

    public ArrayRope<T> append(ArrayRope<T> that) {
      return new ArrayRope<>(new ConcatSegment<>(this.segment, that.segment));
    }

    public ArrayRope<T> append(T... items) {
      return new ArrayRope<T>(new ConcatSegment<>(this.segment, new ArraySegment<>(items)));
    }

    public ArrayRope<T> prepend(T... items) {
      return new ArrayRope<T>(new ConcatSegment<>(new ArraySegment<>(items), this.segment));
    }

    public void writeArray(T[] arr) {
      segment.appendTo(arr, 0);
    }

    public int size() {
      return segment.size();
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
        for (int i = 0; i < elements.length; i++) {
          builder[start + i] = elements[start];
        }
      }

      @Override
      public int size() {
        return elements.length;
      }
    }

    private static final class ConcatSegment<T> implements ArrayRopeSegment<T> {
      private final ArrayRopeSegment<T> left;
      private final ArrayRopeSegment<T> right;
      private int cachedSize = UNKNOWN;
      private static final int UNKNOWN = 1;

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
    }
  }

  public WithWarnings(Object value, Object... warnings) {
    this.warnings = new ArrayRope<>(warnings);
    this.value = value;
  }

  private WithWarnings(Object value, ArrayRope<Object> warnings) {
    this.warnings = warnings;
    this.value = value;
  }

  public Object getValue() {
    return value;
  }

  public WithWarnings inherit(WithWarnings that) {
    return new WithWarnings(value, warnings.append(that.warnings));
  }

  public WithWarnings append(Object... newWarnings) {
    return new WithWarnings(value, warnings.append(newWarnings));
  }

  public WithWarnings prepend(Object newWarnings) {
    return new WithWarnings(value, warnings.prepend(newWarnings));
  }

  public Object[] getWarnings() {
    Object[] result = new Object[warnings.size()];
    warnings.writeArray(result);
    return result;
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }
}
