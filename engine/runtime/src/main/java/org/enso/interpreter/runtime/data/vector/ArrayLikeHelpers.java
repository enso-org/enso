package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.frame.VirtualFrame;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.List;

import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.State;

import com.oracle.truffle.api.CompilerDirectives;

/** Publicly available operations on array-like classes. */
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Internal.Array_Like_Helpers")
public final class ArrayLikeHelpers {
  private ArrayLikeHelpers() {}

  @Builtin.Method(
      name = "new_array_proxy_builtin",
      description = "Creates an array backed by a proxy object.")
  @Builtin.WrapException(from = IllegalArgumentException.class)
  public static EnsoObject create(long length, Object at) throws IllegalArgumentException {
    return ArrayProxy.create(length, at);
  }

  /** Checks whether an array like object is considered immutable.
   * Immutable objects are instances of {@link EnsoObject} and can be safely cast
   * to that interface.
   *
   * @param obj the object to check
   * @return if the {@code obj} is already seen as immutable
   */
  public static boolean isImmutable(Object obj) {
    return obj instanceof Vector;
  }

  /**
   * Takes a slice from an array like object.
   *
   * @param self array like object
   * @param start start of the slice
   * @param end end of the slice
   * @param len the length of the array
   * @return an array-like object representing the slice
   */
  public static Object slice(Object self, long start, long end, long len) {
    var slice = ArraySlice.createOrNull(self, start, len, end);
    return slice == null ? self : slice;
  }

  /**
   * Creates an uninitialized array of the given size.The values must be filled before the array is
   * returned to Enso.
   *
   * @param size the size of the created array.
   * @return the array instance
   */
  public static EnsoObject allocate(long size) {
    var arr = new Object[Math.toIntExact(size)];
    return new Array(arr);
  }

  @Builtin.Method(
      name = "vector_from_function",
      description = "Creates new Vector with given length and provided elements.",
      autoRegister = false)
  @Builtin.Specialize()
  public static Object vectorFromFunction(
      VirtualFrame frame,
      long length,
      Function fun,
      State state,
      @Cached("buildWithArity(1)") InvokeFunctionNode invokeFunctionNode) {
    var len = Math.toIntExact(length);
    var target = new Array_Builder(len);
    for (int i = 0; i < len; i++) {
      var value = invokeFunctionNode.execute(fun, frame, state, new Long[] {(long) i});
      if (value instanceof DataflowError) {
        return value;
      }
      target.add(value);
    }
    var res = target.toArray();
    if (res instanceof long[] longs) {
      return VectorLong.fromArray(longs);
    }
    if (res instanceof double[] doubles) {
      return VectorDouble.fromArray(doubles);
    }
    return Vector.fromArray(new Array((Object[])res));
  }

  @Builtin.Method(
      name = "vector_to_array",
      description = "Returns an Array representation of this Vector.")
  public static Object vectorToArray(Object obj) {
    if (obj instanceof Vector vector) {
      return vector.toArray();
    } else {
      return obj;
    }
  }

  public static EnsoObject wrapBuffer(ByteBuffer buffer) {
    return ArrayOverBuffer.wrapBuffer(buffer);
  }

  public static EnsoObject wrapEnsoObjects(EnsoObject... arr) {
    return new Array((Object[]) arr);
  }

  public static EnsoObject wrapStrings(String... arr) {
    return new Array((Object[]) arr);
  }

  public static EnsoObject wrapObjectsWithCheckAt(Object... arr) {
    return new Array((Object[]) arr);
  }

  public static EnsoObject empty() {
    return allocate(0);
  }

  public static EnsoObject asVectorWithCheckAt(Object... arr) {
    return Vector.fromArray(new Array((Object[]) arr));
  }

  public static EnsoObject asVectorFromArray(Object storage) {
    return Vector.fromArray(storage);
  }

  private static final class Array_Builder<T> {
    private static final Object[] EMPTY_ARRAY = new Object[0];
    private final int initialCapacity;
    private int size;
    private Object primitiveArray;
    private Object[] objectArray;

    private Array_Builder(int initialCapacity) {
      this.initialCapacity = Math.max(1, initialCapacity);
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
    @CompilerDirectives.TruffleBoundary
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
            var arr = new long[initialCapacity];
            arr[0] = l;
            primitiveArray = arr;
          } else if (e instanceof Double d) {
            var arr = new double[initialCapacity];
            arr[0] = d;
            primitiveArray = arr;
          } else {
            var arr = new Object[initialCapacity];
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

    public int getSize() {
      return size;
    }
  }

}
