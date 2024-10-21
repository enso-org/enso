package org.enso.interpreter.runtime.data.vector;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import java.nio.ByteBuffer;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.data.EnsoObject;

/** Publicly available operations on array-like classes. */
@Builtin(pkg = "immutable", stdlibName = "Standard.Base.Internal.Array_Like_Helpers")
public final class ArrayLikeHelpers {
  private ArrayLikeHelpers() {}

  @Builtin.Method(
      name = "new_array_proxy_builtin",
      description = "Creates an array backed by a proxy object.")
  @Builtin.WrapException(from = IllegalArgumentException.class)
  @Builtin.Specialize
  public static EnsoObject create(
      long length, Object at, @CachedLibrary(limit = "3") InteropLibrary interop)
      throws IllegalArgumentException {
    if (!interop.isExecutable(at)) {
      CompilerDirectives.transferToInterpreter();
      var msg = "Array_Proxy needs executable function.";
      throw ArrayPanics.typeError(interop, at, msg);
    }
    if (length < 0) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalArgumentException("Array_Proxy length cannot be negative.");
    }
    return ArrayProxy.create(length, at);
  }

  /**
   * Checks whether an array like object is considered immutable. Immutable objects are instances of
   * {@link EnsoObject} and can be safely cast to that interface.
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
    return Array.allocate(size);
  }

  @Builtin.Method(
      name = "vector_to_array",
      description = "Returns an Array representation of this Vector.")
  @SuppressWarnings("generic-enso-builtin-type")
  public static Object vectorToArray(Object obj) {
    if (obj instanceof Vector.Generic vector) {
      return vector.toArray();
    } else {
      return obj;
    }
  }

  @Builtin.Method(name = "new_vector_builder", description = "Returns new vector builder.")
  @SuppressWarnings("generic-enso-builtin-type")
  public static Object newVectorBuilder(long capacity) {
    return ArrayBuilder.newBuilder((int) Math.min(Math.abs(capacity), Integer.MAX_VALUE));
  }

  public static EnsoObject wrapBuffer(ByteBuffer buffer) {
    return ArrayOverBuffer.wrapBuffer(buffer);
  }

  public static EnsoObject wrapEnsoObjects(EnsoObject... arr) {
    return Array.wrap((Object[]) arr);
  }

  public static EnsoObject wrapStrings(String... arr) {
    return Array.wrap((Object[]) arr);
  }

  public static EnsoObject wrapObjectsWithCheckAt(Object... arr) {
    return Array.wrap((Object[]) arr);
  }

  public static EnsoObject empty() {
    return allocate(0);
  }

  public static EnsoObject asVectorWithCheckAt(Object... arr) {
    return Vector.fromInteropArray(Array.wrap((Object[]) arr));
  }

  public static EnsoObject asVectorFromArray(Object storage) {
    return Vector.fromInteropArray(storage);
  }

  public static EnsoObject asVectorEnsoObjects(EnsoObject... arr) {
    return Vector.fromEnsoOnlyArray(arr);
  }

  public static EnsoObject asVectorEmpty() {
    return Vector.fromEnsoOnlyArray(null);
  }
}
