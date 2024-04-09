package org.enso.interpreter.arrow.util;

import com.oracle.truffle.api.CompilerDirectives;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Constructor;
import java.nio.ByteBuffer;

public final class MemoryUtil {

  private static MethodHandle byteBufferConstr;

  static {
    ByteBuffer buffer = null;
    try {
      buffer = ByteBuffer.allocateDirect(1);
      Constructor<?> constr = buffer.getClass().getDeclaredConstructor(long.class, long.class);
      constr.setAccessible(true);
      byteBufferConstr = MethodHandles.lookup().unreflectConstructor(constr);
    } catch (NoSuchMethodException | IllegalAccessException e) {
      CompilerDirectives.transferToInterpreter();
      throw new ExceptionInInitializerError(
          "Unable to find a constructor for ByteBuffer created directly from a memory address");
    } finally {
      if (buffer != null) {
        buffer.clear();
      }
    }
  }

  private MemoryUtil() {}

  /**
   * Create a ByteBuffer directly from a (allocated) memory address and its size without copying.
   *
   * @param address address in memory to the start of the allocated chunk of memory
   * @param capacity size in bytes of the allocated chunk of memory
   * @return ByteBuffer instance
   */
  public static ByteBuffer directBuffer(long address, long capacity) {
    if (byteBufferConstr != null) {
      try {
        return (ByteBuffer) byteBufferConstr.invoke(address, capacity);
      } catch (Throwable e) {
        CompilerDirectives.transferToInterpreter();
        throw new RuntimeException(e);
      }
    } else {
      CompilerDirectives.transferToInterpreter();
      throw new RuntimeException(
          "constructor for a ByteBuffer created from a memory address is missing");
    }
  }
}
