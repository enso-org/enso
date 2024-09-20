package org.enso.syntax2;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicInteger;

public final class Parser implements AutoCloseable {
  private static void initializeLibraries() {
    try {
      System.loadLibrary("enso_parser");
      return;
    } catch (LinkageError err) {
      // try harder to find the library
    }
    String os = System.getProperty("os.name");
    String name;
    if (os.startsWith("Mac")) {
      name = "libenso_parser.dylib";
    } else if (os.startsWith("Windows")) {
      name = "enso_parser.dll";
    } else {
      name = "libenso_parser.so";
    }

    var whereAmI = Parser.class.getProtectionDomain().getCodeSource().getLocation();
    File root;
    try {
      root = new File(whereAmI.toURI()).getParentFile();
    } catch (URISyntaxException ex) {
      root = new File(".").getAbsoluteFile();
    }
    try {
      var d = root;
      File path = null;
      while (d != null) {
        path = new File(new File(d, "component"), name);
        if (path.exists()) break;
        d = d.getParentFile();
      }
      if (d == null || path == null) {
        throw new LinkageError("Cannot find parser in " + root);
      }
      System.load(path.getAbsolutePath());
    } catch (NullPointerException | IllegalArgumentException | LinkageError e) {
      if (searchFromDirToTop(e, root, "target", "rust", "parser-jni", name)) {
        return;
      }
      if (searchFromDirToTop(
          e, new File(".").getAbsoluteFile(), "target", "rust", "parser-jni", name)) {
        return;
      }
      throw new IllegalStateException("Cannot load parser from " + root, e);
    }
  }

  private static boolean searchFromDirToTop(Throwable chain, File root, String... names) {
    while (root != null) {
      var parser = root;
      for (var e : names) {
        parser = new File(parser, e);
      }
      try {
        System.load(parser.getAbsolutePath());
        return true;
      } catch (LinkageError err) {
        while (chain.getCause() != null) {
          chain = chain.getCause();
        }
        chain.initCause(err);
        root = root.getParentFile();
      }
    }
    return false;
  }

  private long stateUnlessClosed;
  private AtomicInteger mutators = new AtomicInteger(0);

  private Parser(long stateIn) {
    stateUnlessClosed = stateIn;
  }

  private static native long allocState();

  private static native void freeState(long state);

  private static native ByteBuffer parseTree(long state, ByteBuffer input);

  private static native ByteBuffer parseTreeLazy(long state, ByteBuffer input);

  private static native long isIdentOrOperator(ByteBuffer input);

  private static native long getLastInputBase(long state);

  private static native long getMetadata(long state);

  private static native String getWarningTemplate(int warningId);

  static native long getUuidHigh(long metadata, long codeOffset, long codeLength);

  static native long getUuidLow(long metadata, long codeOffset, long codeLength);

  public static Parser create() {
    initializeLibraries();
    var state = allocState();
    return new Parser(state);
  }

  public long isIdentOrOperator(CharSequence input) {
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);

    return isIdentOrOperator(inputBuf);
  }

  private long getState() {
    if (stateUnlessClosed != 0) {
      return stateUnlessClosed;
    } else {
      throw new IllegalStateException("Parser used after close()");
    }
  }

  public ByteBuffer parseInputLazy(CharSequence input) {
    if (mutators.get() != 0) throw new IllegalStateException("Race condition detected");
    mutators.getAndIncrement();
    var state = getState();
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);
    var result = parseTreeLazy(state, inputBuf);
    mutators.getAndDecrement();
    return result;
  }

  public Tree parse(CharSequence input) {
    if (mutators.get() != 0) throw new IllegalStateException("Race condition detected");
    mutators.getAndIncrement();
    var state = getState();
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);
    var serializedTree = parseTree(state, inputBuf);
    var base = getLastInputBase(state);
    var metadata = getMetadata(state);
    serializedTree.order(ByteOrder.LITTLE_ENDIAN);
    var message = new Message(serializedTree, input, base, metadata);
    var result = Tree.deserialize(message);
    mutators.getAndDecrement();
    return result;
  }

  public static String getWarningMessage(Warning warning) {
    return getWarningTemplate(warning.getId());
  }

  @Override
  public void close() {
    freeState(stateUnlessClosed);
    stateUnlessClosed = 0;
  }
}
