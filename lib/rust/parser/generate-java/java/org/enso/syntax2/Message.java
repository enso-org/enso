package org.enso.syntax2;

import java.nio.charset.StandardCharsets;

final class Message {
  private final java.nio.ByteBuffer buffer;
  private final CharSequence context;
  private final int base;
  private final long metadata;
  private long position;

  Message(java.nio.ByteBuffer bufferIn, CharSequence contextIn, long baseIn, long metadataIn) {
    buffer = bufferIn;
    context = contextIn;
    base = (int) baseIn;
    metadata = metadataIn;
  }

  long get64() {
    return buffer.getLong();
  }

  int get32() {
    return buffer.getInt();
  }

  boolean getBoolean() {
    switch (buffer.get()) {
      case 0:
        return false;
      case 1:
        return true;
      default:
        throw new FormatException("Boolean out of range");
    }
  }

  String getString() {
    int len = (int) get64();
    byte[] dst = new byte[len];
    buffer.get(dst);
    return new String(dst, StandardCharsets.UTF_8);
  }

  CharSequence context() {
    return context;
  }

  int offset(int xLow32) {
    // Given the low bits of `x`, the low bits of `base`, and the invariant `x >= base`,
    // return `x - base`.
    int tmp = xLow32 - base;
    if (tmp < 0) {
      tmp += Integer.MAX_VALUE;
      tmp += 1;
    }
    return tmp;
  }

  String getLocation() {
    return "Message[buffer=" + buffer.position() + "]";
  }

  java.util.UUID getUuid(long nodeOffset, long nodeLength) {
    return Parser.getUuid(metadata, nodeOffset, nodeLength);
  }

  long position() {
    return position;
  }

  long advance(long delta) {
    position += delta;
    return position;
  }
}
