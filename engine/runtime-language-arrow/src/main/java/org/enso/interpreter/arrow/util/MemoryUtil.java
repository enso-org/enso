package org.enso.interpreter.arrow.util;

import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;

public final class MemoryUtil {
  private MemoryUtil() {}

  /**
   * Create a ByteBuffer directly from a (allocated) memory address and its size without copying.
   *
   * @param address address in memory to the start of the allocated chunk of memory
   * @param capacity size in bytes of the allocated chunk of memory
   * @return ByteBuffer instance
   */
  public static ByteBuffer directBuffer(long address, long capacity) {
    var seg = MemorySegment.ofAddress(address).reinterpret(capacity);
    return seg.asByteBuffer();
  }
}
