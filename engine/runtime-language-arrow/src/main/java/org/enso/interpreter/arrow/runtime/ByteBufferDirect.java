package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.nio.ByteBuffer;

final class ByteBufferDirect {
  private final ByteBuffer buffer;
  private final ByteBuffer nonNullBitmap;

  /**
   * Creates a fresh buffer with an empty non-null bitmap..
   *
   * @param sizeInBytes size of the new buffer in bytes
   * @param size size of the new buffer for the elements of the requested type
   */
  public ByteBufferDirect(int sizeInBytes, int size) {
    this.buffer = ByteBuffer.allocate(sizeInBytes);
    this.nonNullBitmap = ByteBuffer.allocate((int) Math.ceil(size / 8) + 1);
    for (int i = 0; i < nonNullBitmap.capacity(); i++) {
      nonNullBitmap.put(i, (byte) 0);
    }
  }

  /**
   * Creates a new buffer from memory-mapped buffer and a corresponding memory-mapped non-null
   * bitmap.
   *
   * @param buffer memory-mapped buffer
   * @param nonNullBitmap memory-mapped buffer representing non-null bitmap
   */
  public ByteBufferDirect(ByteBuffer buffer, ByteBuffer nonNullBitmap) {
    this.buffer = buffer;
    this.nonNullBitmap = nonNullBitmap;
  }

  /**
   * Creates a new buffer from memory-mapped buffer of a given size assuming it has only non-null
   * values.
   *
   * @param buffer memory-mapped buffer
   * @param size size of the buffer (in bytes)
   */
  public ByteBufferDirect(ByteBuffer buffer, int size) {
    this.buffer = buffer;
    this.nonNullBitmap = ByteBuffer.allocate((int) Math.ceil(size / 8) + 1);
    for (int i = 0; i < nonNullBitmap.capacity(); i++) {
      nonNullBitmap.put(i, (byte) 255);
    }
  }

  public void put(byte b) throws UnsupportedMessageException {
    setValidityBitmap(0, 1);
    buffer.put(b);
  }

  public byte get(int index) throws UnsupportedMessageException {
    return buffer.get(index);
  }

  public void put(int index, byte b) throws UnsupportedMessageException {
    setValidityBitmap(index, 1);
    buffer.put(index, b);
  }

  public void putShort(short value) throws UnsupportedMessageException {
    setValidityBitmap(0, 2);
    buffer.putShort(value);
  }

  public short getShort(int index) throws UnsupportedMessageException {
    return buffer.getShort(index);
  }

  public void putShort(int index, short value) throws UnsupportedMessageException {
    setValidityBitmap(index, 2);
    buffer.putShort(index, value);
  }

  public void putInt(int value) throws UnsupportedMessageException {
    setValidityBitmap(0, 4);
    buffer.putInt(value);
  }

  public int getInt(int index) throws UnsupportedMessageException {
    return buffer.getInt(index);
  }

  public void putInt(int index, int value) throws UnsupportedMessageException {
    setValidityBitmap(index, 4);
    buffer.putInt(index, value);
  }

  public void putLong(long value) throws UnsupportedMessageException {
    setValidityBitmap(0, 8);
    buffer.putLong(value);
  }

  public long getLong(int index) throws UnsupportedMessageException {
    return buffer.getLong(index);
  }

  public void putLong(int index, long value) throws UnsupportedMessageException {
    setValidityBitmap(index, 8);
    buffer.putLong(index, value);
  }

  public void putFloat(float value) throws UnsupportedMessageException {
    setValidityBitmap(0, 4);
    buffer.putFloat(value);
  }

  public float getFloat(int index) throws UnsupportedMessageException {
    return buffer.getFloat(index);
  }

  public void putFloat(int index, float value) throws UnsupportedMessageException {
    setValidityBitmap(index, 4);
    buffer.putFloat(index, value);
  }

  public void putDouble(double value) throws UnsupportedMessageException {
    setValidityBitmap(0, 8);
    buffer.putDouble(value);
  }

  public double getDouble(int index) throws UnsupportedMessageException {
    return buffer.getDouble(index);
  }

  public void putDouble(int index, double value) throws UnsupportedMessageException {
    setValidityBitmap(index, 8);
    buffer.putDouble(index, value);
  }

  public int capacity() throws UnsupportedMessageException {
    return buffer.capacity();
  }

  public boolean isNull(int index) {
    var bufferIndex = index >> 3;
    var byteIndex = index & ~(1 << 3);
    var slot = nonNullBitmap.get(bufferIndex);
    var mask = 1 << byteIndex;
    return (slot & mask) == 0;
  }

  public void setNull(int index) {
    var bufferIndex = index >> 3;
    var byteIndex = index & ~(1 << 3);
    var slot = nonNullBitmap.get(bufferIndex);
    var mask = ~(1 << byteIndex);
    nonNullBitmap.put(bufferIndex, (byte) (slot & mask));
  }

  private void setValidityBitmap(int index0, int unitSize) {
    var index = index0 / unitSize;
    var bufferIndex = index >> 3;
    var byteIndex = index & ~(1 << 3);
    var slot = nonNullBitmap.get(bufferIndex);
    var mask = 1 << byteIndex;
    var updated = (slot | mask);
    nonNullBitmap.put(bufferIndex, (byte) (updated));
  }
}
