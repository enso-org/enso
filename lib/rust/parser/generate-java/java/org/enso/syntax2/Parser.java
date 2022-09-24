package org.enso.syntax2;

import org.enso.syntax2.Message;
import org.enso.syntax2.UnsupportedSyntaxException;
import java.io.File;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;

public final class Parser implements AutoCloseable {
  static {
    String os = System.getProperty("os.name");
    File dir = new File(".").getAbsoluteFile();
    for (; ; ) {
      File parser;
      if (os.startsWith("Mac")) {
        parser = new File(dir, "target/rust/debug/libenso_parser.dylib");
      } else if (os.startsWith("Windows")) {
        parser = new File(dir, "target/rust/debug/enso_parser.dll");
      } else {
        parser = new File(dir, "target/rust/debug/libenso_parser.so");
      }
      try {
        System.load(parser.getAbsolutePath());
        break;
      } catch (LinkageError e) {
        dir = dir.getParentFile();
        if (dir == null) {
          throw e;
        }
      }
    }
  }

  private long state;

  private Parser(long stateIn) {
    state = stateIn;
  }

  private static native long allocState();

  private static native void freeState(long state);

  private static native ByteBuffer parseInput(long state, ByteBuffer input);

  private static native long getLastInputBase(long state);

  private static native long getMetadata(long state);

  static native long getUuidHigh(long metadata, long codeOffset, long codeLength);

  static native long getUuidLow(long metadata, long codeOffset, long codeLength);

  public static Parser create() {
    var state = allocState();
    return new Parser(state);
  }

  public Tree parse(CharSequence input) throws UnsupportedSyntaxException {
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);
    var serializedTree = parseInput(state, inputBuf);
    var base = getLastInputBase(state);
    var metadata = getMetadata(state);
    serializedTree.order(ByteOrder.LITTLE_ENDIAN);
    var message = new Message(serializedTree, input, base, metadata);
    var result = Tree.deserialize(message);
    if (message.getEncounteredUnsupportedSyntax()) {
      throw new UnsupportedSyntaxException(result);
    }
    return result;
  }

  @Override
  public void close() {
    freeState(state);
    state = 0;
  }
}
