package org.enso.syntax2;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;

public final class Parser implements AutoCloseable {
  static {
    String os = System.getProperty("os.name");
    String name;
    if (os.startsWith("Mac")) {
      name = "libenso_parser.dylib";
    } else if (os.startsWith("Windows")) {
      name = "enso_parser.dll";
    } else {
      name = "libenso_parser.so";
    }

    File parser = null;
    try {
      var whereAmI = Parser.class.getProtectionDomain().getCodeSource().getLocation();
      File dir = new File(new File(whereAmI.toURI()).getParentFile().getParentFile(), "native-libraries");
      parser = new File(dir, name);
      System.load(parser.getAbsolutePath());
    } catch (URISyntaxException | LinkageError e) {
      System.err.println("Cannot load " + parser);
      File root = new File(".").getAbsoluteFile();
      for (var dir = new File(new File(new File(root, "target"), "rust"), "debug");;) {
        parser = new File(dir, name);
        try {
          System.load(parser.getAbsolutePath());
          System.err.println("Succeeded loading " + parser.getAbsolutePath());
          break;
        } catch (LinkageError err) {
          dir = dir.getParentFile();
          if (dir == null) {
            throw err;
          }
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

  public Tree parse(CharSequence input) {
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);
    var serializedTree = parseInput(state, inputBuf);
    var base = getLastInputBase(state);
    var metadata = getMetadata(state);
    serializedTree.order(ByteOrder.LITTLE_ENDIAN);
    var message = new Message(serializedTree, input, base, metadata);
    return Tree.deserialize(message);
  }

  @Override
  public void close() {
    freeState(state);
    state = 0;
  }
}
