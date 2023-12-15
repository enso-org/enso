package org.enso.interpreter.arrow.runtime;

import com.oracle.truffle.api.interop.UnsupportedMessageException;

public abstract class ByteBufferProxy {
  public abstract void put(byte b) throws UnsupportedMessageException;

  public abstract byte get(int index) throws UnsupportedMessageException;

  public abstract void put(int index, byte b) throws UnsupportedMessageException;

  public abstract void putShort(short value) throws UnsupportedMessageException;

  public abstract short getShort(int index) throws UnsupportedMessageException;

  public abstract void putShort(int index, short value) throws UnsupportedMessageException;

  public abstract void putInt(int value) throws UnsupportedMessageException;

  public abstract int getInt(int index) throws UnsupportedMessageException;

  public abstract void putInt(int index, int value) throws UnsupportedMessageException;

  public abstract void putLong(long value) throws UnsupportedMessageException;

  public abstract long getLong(int index) throws UnsupportedMessageException;

  public abstract void putLong(int index, long value) throws UnsupportedMessageException;

  public abstract void putFloat(float value) throws UnsupportedMessageException;

  public abstract float getFloat(int index) throws UnsupportedMessageException;

  public abstract void putFloat(int index, float value) throws UnsupportedMessageException;

  public abstract void putDouble(double value) throws UnsupportedMessageException;

  public abstract double getDouble(int index) throws UnsupportedMessageException;

  public abstract void putDouble(int index, double value) throws UnsupportedMessageException;

  public abstract int capacity() throws UnsupportedMessageException;

  public abstract boolean isNull(int index);

  public abstract void setNull(int index);
}
