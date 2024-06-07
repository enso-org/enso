package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.profiles.InlinedExactClassProfile;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import org.enso.interpreter.arrow.LogicalLayout;
import org.enso.interpreter.arrow.util.MemoryUtil;

final class ByteBufferDirect implements AutoCloseable {
  private final ByteBuffer allocated;
  final ByteBuffer dataBuffer;
  private ByteBuffer bitmapBuffer;

  /**
   * Creates a fresh buffer with an empty non-null bitmap..
   *
   * @param valueCount number of elements in the buffer
   * @param unit size of the new buffer for the elements of the requested type
   */
  private ByteBufferDirect(int valueCount, SizeInBytes unit) {
    var padded = RoundingUtil.forValueCount(valueCount, unit);
    var buffer = ByteBuffer.allocate(padded.getTotalSizeInBytes());

    this.allocated = buffer;
    this.dataBuffer = buffer.slice(0, padded.getDataBufferSizeInBytes());
    this.bitmapBuffer = null;
  }

  /**
   * Creates a new buffer from memory-mapped buffer and a corresponding memory-mapped non-null
   * bitmap.
   *
   * @param dataBuffer memory-mapped data buffer
   * @param bitmapBuffer memory-mapped buffer representing null bitmaps
   */
  private ByteBufferDirect(ByteBuffer allocated, ByteBuffer dataBuffer, ByteBuffer bitmapBuffer) {
    this.allocated = allocated;
    this.dataBuffer = dataBuffer;
    this.bitmapBuffer = bitmapBuffer;
  }

  /**
   * Creates a new buffer from memory-mapped buffer of a given size assuming it has only non-null
   * values.
   *
   * @param dataBuffer memory-mapped buffer
   * @param bitmapSizeInBytes size of the bitmap buffer (in bytes)
   */
  private ByteBufferDirect(ByteBuffer allocated, ByteBuffer dataBuffer, int bitmapSizeInBytes) {
    this.allocated = allocated;
    this.dataBuffer = dataBuffer;
    this.bitmapBuffer = allocated.slice(dataBuffer.capacity(), bitmapSizeInBytes);
    for (int i = 0; i < bitmapBuffer.capacity(); i++) {
      bitmapBuffer.put(i, (byte) 255);
    }
  }

  static ByteBufferDirect forBuffer(ByteBuffer buf) {
    return new ByteBufferDirect(buf, buf, null);
  }

  /**
   * Creates a new buffer being able to store `valueCount` number of elements.
   *
   * @param valueCount number of elements to store in the buffer
   * @param unit size of a single element in bytes
   */
  public static ByteBufferDirect forSize(int valueCount, SizeInBytes unit) {
    return new ByteBufferDirect(valueCount, unit);
  }

  /**
   * Creates a new buffer from a given memory address range..
   *
   * @param dataAddress address of a data buffer
   * @param unit size of a single element in bytes
   */
  public static ByteBufferDirect fromAddress(long dataAddress, int valueCount, SizeInBytes unit) {
    var padded = RoundingUtil.forValueCount(valueCount, unit);
    ByteBuffer buffer = MemoryUtil.directBuffer(dataAddress, padded.getTotalSizeInBytes());
    ByteBuffer dataBuffer = buffer.slice(0, padded.getDataBufferSizeInBytes());
    dataBuffer.order(ByteOrder.LITTLE_ENDIAN);
    return new ByteBufferDirect(buffer, dataBuffer, padded.getValidityBitmapSizeInBytes());
  }

  public static ByteBufferDirect fromAddress(
      long dataAddress, long bitmapAddress, int size, SizeInBytes unit) {
    var padded = RoundingUtil.forValueCount(size, unit);
    ByteBuffer allocated = MemoryUtil.directBuffer(dataAddress, padded.getTotalSizeInBytes());
    assert dataAddress + padded.getDataBufferSizeInBytes() == bitmapAddress;
    ByteBuffer dataBuffer = allocated.slice(0, padded.getDataBufferSizeInBytes());
    dataBuffer.order(ByteOrder.LITTLE_ENDIAN);
    ByteBuffer bitmapBuffer =
        allocated.slice(dataBuffer.capacity(), padded.getValidityBitmapSizeInBytes());
    return new ByteBufferDirect(allocated, dataBuffer, bitmapBuffer);
  }

  public void put(byte b) throws UnsupportedMessageException {
    setValidityBitmap(0, 1);
    dataBuffer.put(b);
  }

  void putNull(LogicalLayout unit) {
    var index = dataBuffer.position() / unit.sizeInBytes();
    setNull(index);
    dataBuffer.position(dataBuffer.position() + unit.sizeInBytes());
  }

  public byte get(int index) throws UnsupportedMessageException {
    return dataBuffer.get(index);
  }

  public void put(int index, byte b) throws UnsupportedMessageException {
    setValidityBitmap(index, 1);
    dataBuffer.put(index, b);
  }

  public void putShort(short value) throws UnsupportedMessageException {
    setValidityBitmap(0, 2);
    dataBuffer.putShort(value);
  }

  public short getShort(int index) throws UnsupportedMessageException {
    return dataBuffer.getShort(index);
  }

  public void putShort(int index, short value) throws UnsupportedMessageException {
    setValidityBitmap(index, 2);
    dataBuffer.putShort(index, value);
  }

  public void putInt(int value) throws UnsupportedMessageException {
    setValidityBitmap(0, 4);
    dataBuffer.putInt(value);
  }

  public int getInt(int index) throws UnsupportedMessageException {
    return dataBuffer.getInt(index);
  }

  public void putInt(int index, int value) {
    setValidityBitmap(index, 4);
    dataBuffer.putInt(index, value);
  }

  public void putLong(long value) throws UnsupportedMessageException {
    setValidityBitmap(0, 8);
    dataBuffer.putLong(value);
  }

  public long getLong(int index) throws UnsupportedMessageException {
    return dataBuffer.getLong(index);
  }

  public long getLong(int index, Node node, InlinedExactClassProfile profile)
      throws UnsupportedMessageException {
    var buf = profile.profile(node, dataBuffer);
    return buf.getLong(index);
  }

  public void putLong(int index, long value) {
    setValidityBitmap(index, 8);
    dataBuffer.putLong(index, value);
  }

  public void putLong(int index, long value, Node node, InlinedExactClassProfile profile) {
    var buf = profile.profile(node, dataBuffer);
    setValidityBitmap(index, 8);
    buf.putLong(index, value);
  }

  public void putFloat(float value) throws UnsupportedMessageException {
    setValidityBitmap(0, 4);
    dataBuffer.putFloat(value);
  }

  public float getFloat(int index) throws UnsupportedMessageException {
    return dataBuffer.getFloat(index);
  }

  public void putFloat(int index, float value) throws UnsupportedMessageException {
    setValidityBitmap(index, 4);
    dataBuffer.putFloat(index, value);
  }

  public void putDouble(double value) throws UnsupportedMessageException {
    setValidityBitmap(0, 8);
    dataBuffer.putDouble(value);
  }

  public double getDouble(int index) throws UnsupportedMessageException {
    return dataBuffer.getDouble(index);
  }

  public void putDouble(int index, double value) throws UnsupportedMessageException {
    setValidityBitmap(index, 8);
    dataBuffer.putDouble(index, value);
  }

  public int capacity() throws UnsupportedMessageException {
    return dataBuffer.capacity();
  }

  public boolean isNull(int index) {
    if (bitmapBuffer == null) {
      return false;
    }
    return checkForNull(index);
  }

  @CompilerDirectives.TruffleBoundary
  private boolean checkForNull(int index) {
    var bufferIndex = index >> 3;
    var slot = bitmapBuffer.get(bufferIndex);
    var byteIndex = index & byteMask;
    var mask = 1 << byteIndex;
    return (slot & mask) == 0;
  }

  public void setNull(int index) {
    if (bitmapBuffer == null) {
      this.bitmapBuffer =
          allocated.slice(dataBuffer.capacity(), allocated.capacity() - dataBuffer.capacity());
      for (var i = 0; i < bitmapBuffer.capacity(); i++) {
        bitmapBuffer.put(i, (byte) 0xff);
      }
    }
    var bufferIndex = index >> 3;
    var slot = bitmapBuffer.get(bufferIndex);
    var byteIndex = index & byteMask;
    var mask = ~(1 << byteIndex);
    bitmapBuffer.put(bufferIndex, (byte) (slot & mask));
  }

  private void setValidityBitmap(int index0, int unitSize) {
    if (bitmapBuffer == null) {
      // all non-null
      return;
    }
    var index = index0 / unitSize;
    var bufferIndex = index >> 3;
    var slot = bitmapBuffer.get(bufferIndex);
    var byteIndex = index & byteMask;

    var mask = 1 << byteIndex;
    var updated = (slot | mask);
    bitmapBuffer.put(bufferIndex, (byte) (updated));
  }

  private static final int byteMask = ~(~(1 << 3) + 1); // 7

  @Override
  public void close() throws Exception {
    this.dataBuffer.clear();
    this.bitmapBuffer.clear();
    this.allocated.clear();
  }
}
