package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.nio.ByteBuffer;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.state.State;

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
   * @return
   */
  public static Array allocate(long size) {
    var arr = new Object[(int) size];
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
    Object[] target = new Object[Math.toIntExact(length)];
    for (int i = 0; i < target.length; i++) {
      var value = invokeFunctionNode.execute(fun, frame, state, new Long[] {(long) i});
      if (value instanceof DataflowError) {
        return value;
      }
      target[i] = value;
    }
    return Vector.fromArray(new Array(target));
  }

  @Builtin.Method(
      name = "vector_to_array",
      description = "Returns an Array representation of this Vector.")
  public static Object vectorToArray(Vector vector) {
    return vector.toArray();
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
}
