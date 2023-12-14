package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.interop.UnsupportedMessageException;
import java.nio.ByteBuffer;

public class ByteBufferDirect extends ByteBufferProxy {
  private final ByteBuffer buffer;

  public ByteBufferDirect(int sizeInBytes, int size) {
    this.buffer = ByteBuffer.allocate(sizeInBytes);
  }

  public ByteBufferDirect(ByteBuffer buffer, int size) {
    this.buffer = buffer;
  }

  @Override
  public void put(byte b) throws UnsupportedMessageException {
    setValidityBitmap(0);
    buffer.put(b);
  }

  @Override
  public byte get(int index) throws UnsupportedMessageException {
    return buffer.get(index);
  }

  @Override
  public void put(int index, byte b) throws UnsupportedMessageException {
    setValidityBitmap(index);
    buffer.put(index, b);
  }

  @Override
  public void putShort(short value) throws UnsupportedMessageException {
    setValidityBitmap(0);
    buffer.putShort(value);
  }

  @Override
  public short getShort(int index) throws UnsupportedMessageException {
    return buffer.getShort(index);
  }

  @Override
  public void putShort(int index, short value) throws UnsupportedMessageException {
    setValidityBitmap(index);
    buffer.putShort(index, value);
  }

  @Override
  public void putInt(int value) throws UnsupportedMessageException {
    setValidityBitmap(0);
    buffer.putInt(value);
  }

  @Override
  public int getInt(int index) throws UnsupportedMessageException {
    return buffer.getInt(index);
  }

  @Override
  public void putInt(int index, int value) throws UnsupportedMessageException {
    setValidityBitmap(index);
    buffer.putInt(index, value);
  }

  @Override
  public void putLong(long value) throws UnsupportedMessageException {
    setValidityBitmap(0);
    buffer.putLong(value);
  }

  @Override
  public long getLong(int index) throws UnsupportedMessageException {
    return buffer.getLong(index);
  }

  @Override
  public void putLong(int index, long value) throws UnsupportedMessageException {
    setValidityBitmap(index);
    buffer.putLong(index, value);
  }

  @Override
  public void putFloat(float value) throws UnsupportedMessageException {
    setValidityBitmap(0);
    buffer.putFloat(value);
  }

  @Override
  public float getFloat(int index) throws UnsupportedMessageException {
    return buffer.getFloat(index);
  }

  @Override
  public void putFloat(int index, float value) throws UnsupportedMessageException {
    setValidityBitmap(index);
    buffer.putFloat(index, value);
  }

  @Override
  public void putDouble(double value) throws UnsupportedMessageException {
    setValidityBitmap(0);
    buffer.putDouble(value);
  }

  @Override
  public double getDouble(int index) throws UnsupportedMessageException {
    return buffer.getDouble(index);
  }

  @Override
  public void putDouble(int index, double value) throws UnsupportedMessageException {
    setValidityBitmap(index);
    buffer.putDouble(index, value);
  }

  @Override
  public int capacity() throws UnsupportedMessageException {
    return buffer.capacity();
  }

  @Override
  public boolean isNull(int index) {
    // TODO
    return false;
  }

  private void setValidityBitmap(int index) {
    // TODO
  }
}
