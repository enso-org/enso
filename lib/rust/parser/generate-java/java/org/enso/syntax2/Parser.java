package org.enso.syntax2;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;

public final class Parser implements AutoCloseable {
  private long state;

  private Parser(long stateIn) {
    state = stateIn;
  }

  private static long allocState() {
    return 0;
  }

  private static void freeState(long state) {}

  private static ByteBuffer parseInput(long state, ByteBuffer input) {
    // fake output representing simple Enso source:
    // main = 42
    return ByteBuffer.wrap(
        new byte[] {
          0, 0, 0, 0, -95, 68, 55, 103, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0,
          1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -42, 67, 55, 103, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          -95, 68, 55, 103, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, -42, 67, 55, 103, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 0, 0, 19, 0, 0, 0, 0, 0, 0, 0, -42, 67, 55, 103, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, -42, 67, 55, 103, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 32, -12, 58, -76, 4, 0, 0, 0, 0, 0, 0, 0, 4, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 36, -12, 58, -76, 1, 0, 0, 0, 4, 0, 0, 0, 1, 0, 0, 0, 37,
          -12, 58, -76, 1, 0, 0, 0, 5, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 38, -12, 58, -76, 1, 0, 0,
          0, 6, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 6, 0, 0, 0, 0, 1, 0, 0, 0, 0, -42, 67, 55, 103, 0,
          0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 39, -12, 58, -76, 2, 0, 0, 0, 7, 0, 0, 0, 2, 0, 0, 0, 0,
          0
        });
  }

  private static long getLastInputBase(long state) {
    return 139726899835936L;
  }

  private static long getMetadata(long state) {
    return 0L;
  }

  static long getUuidHigh(long metadata, long codeOffset, long codeLength) {
    return 0L;
  }

  static long getUuidLow(long metadata, long codeOffset, long codeLength) {
    return 0L;
  }

  public static Parser create() {
    var state = allocState();
    return new Parser(state);
  }

  public Tree parse(CharSequence input) {
    var serializedTree = parseInput(state, null);
    var base = getLastInputBase(state);
    var metadata = getMetadata(state);
    try {
      serializedTree.order(ByteOrder.LITTLE_ENDIAN);
    } catch (Throwable ex) {
    }
    var message = new Message(serializedTree, input, base, metadata);
    return Tree.deserialize(message);
  }

  @Override
  public void close() {
    freeState(state);
    state = 0;
  }
}
