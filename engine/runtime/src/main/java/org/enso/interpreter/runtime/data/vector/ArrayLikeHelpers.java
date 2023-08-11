package org.enso.interpreter.runtime.data.vector;

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
  public static EnsoObject create(long length, Object at) throws IllegalArgumentException {
    return ArrayProxy.create(length, at);
  }

  public static EnsoObject wrapBuffer(ByteBuffer buffer) {
    return ArrayOverBuffer.wrapBuffer(buffer);
  }
}
