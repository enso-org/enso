package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.nio.ByteBuffer;
import java.util.BitSet;

final class ByteBufferDirect extends ByteBufferProxy {
  private final ByteBuffer buffer;
  private final BitSet validityMap;

  public ByteBufferDirect(int sizeInBytes, int size) {
    this.buffer = ByteBuffer.allocate(sizeInBytes);
    this.validityMap = new BitSet(size);
  }

  public ByteBufferDirect(ByteBuffer buffer, BitSet validityMap) {
    this.buffer = buffer;
    this.validityMap = validityMap;
  }

  public ByteBufferDirect(ByteBuffer buffer, int size) {
    this.buffer = buffer;
    this.validityMap = new BitSet(size);
    validityMap.set(0, validityMap.size() - 1, true);
  }

  @Override
  public void put(byte b) throws UnsupportedMessageException {
    setValidityBitmap(0, 1);
    buffer.put(b);
  }

  @Override
  public byte get(int index) throws UnsupportedMessageException {
    return buffer.get(index);
  }

  @Override
  public void put(int index, byte b) throws UnsupportedMessageException {
    setValidityBitmap(index, 1);
    buffer.put(index, b);
  }

  @Override
  public void putShort(short value) throws UnsupportedMessageException {
    setValidityBitmap(0, 2);
    buffer.putShort(value);
  }

  @Override
  public short getShort(int index) throws UnsupportedMessageException {
    return buffer.getShort(index);
  }

  @Override
  public void putShort(int index, short value) throws UnsupportedMessageException {
    setValidityBitmap(index, 2);
    buffer.putShort(index, value);
  }

  @Override
  public void putInt(int value) throws UnsupportedMessageException {
    setValidityBitmap(0, 4);
    buffer.putInt(value);
  }

  @Override
  public int getInt(int index) throws UnsupportedMessageException {
    return buffer.getInt(index);
  }

  @Override
  public void putInt(int index, int value) throws UnsupportedMessageException {
    setValidityBitmap(index, 4);
    buffer.putInt(index, value);
  }

  @Override
  public void putLong(long value) throws UnsupportedMessageException {
    setValidityBitmap(0, 8);
    buffer.putLong(value);
  }

  @Override
  public long getLong(int index) throws UnsupportedMessageException {
    return buffer.getLong(index);
  }

  @Override
  public void putLong(int index, long value) throws UnsupportedMessageException {
    setValidityBitmap(index, 8);
    buffer.putLong(index, value);
  }

  @Override
  public void putFloat(float value) throws UnsupportedMessageException {
    setValidityBitmap(0, 4);
    buffer.putFloat(value);
  }

  @Override
  public float getFloat(int index) throws UnsupportedMessageException {
    return buffer.getFloat(index);
  }

  @Override
  public void putFloat(int index, float value) throws UnsupportedMessageException {
    setValidityBitmap(index, 4);
    buffer.putFloat(index, value);
  }

  @Override
  public void putDouble(double value) throws UnsupportedMessageException {
    setValidityBitmap(0, 8);
    buffer.putDouble(value);
  }

  @Override
  public double getDouble(int index) throws UnsupportedMessageException {
    return buffer.getDouble(index);
  }

  @Override
  public void putDouble(int index, double value) throws UnsupportedMessageException {
    setValidityBitmap(index, 8);
    buffer.putDouble(index, value);
  }

  @Override
  public int capacity() throws UnsupportedMessageException {
    return buffer.capacity();
  }

  @Override
  public boolean isNull(int index) {
    return !validityMap.get(index);
  }

  @Override
  public void setNull(int index) {
    validityMap.set(index, false);
  }

  private void setValidityBitmap(int index, int unitSize) {
    validityMap.set(index / unitSize);
  }
}
