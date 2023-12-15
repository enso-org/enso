package org.enso.interpreter.arrow.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.nio.ByteBuffer;

public class MemoryUtil {

  private static Constructor<?> byteBufferConstr = null;

  static {
    ByteBuffer buffer = null;
    try {
      buffer = ByteBuffer.allocateDirect(1);
      byteBufferConstr = buffer.getClass().getDeclaredConstructor(long.class, long.class);
      byteBufferConstr.setAccessible(true);
    } catch (NoSuchMethodException e) {
      System.err.println(
          "Unable to find a constructor for ByteBuffer created directly from a memory address: "
              + e.getMessage());
    } finally {
      if (buffer != null) {
        buffer.clear();
      }
    }
  }

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
        return (ByteBuffer) byteBufferConstr.newInstance(address, capacity);
      } catch (InstantiationException e) {
        throw new RuntimeException(e);
      } catch (IllegalAccessException e) {
        throw new RuntimeException(e);
      } catch (InvocationTargetException e) {
        throw new RuntimeException(e);
      }
    }
    throw new RuntimeException(
        "constructor for a ByteBuffer created from a memory address is missing");
  }
}
