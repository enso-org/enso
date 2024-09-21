package org.enso.syntax2;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicBoolean;

public final class Parser {
  static AtomicBoolean initialized = new AtomicBoolean(false);

  private static void ensureInitialized() {
    // Optimization: Skip finding and initializing the library when it's definitely already been
    // done.
    // Note that this inexpensive check may produce false negatives: Nothing prevents multiple
    // threads from proceeding to library initialization concurrently (before any thread has
    // completed initialization), but that is harmless; library initialization is thread-safe and
    // idempotent.
    if (initialized.get()) return;
    try {
      System.loadLibrary("enso_parser");
      initialized.set(true);
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
        initialized.set(true);
        return;
      }
      if (searchFromDirToTop(
          e, new File(".").getAbsoluteFile(), "target", "rust", "parser-jni", name)) {
        initialized.set(true);
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

  private Parser() {}

  private static native void freeBuffers();

  private static native ByteBuffer parseTree(ByteBuffer input);

  private static native ByteBuffer parseTreeLazy(ByteBuffer input);

  private static native long isIdentOrOperator(ByteBuffer input);

  private static native long getLastInputBase();

  private static native long getMetadata();

  private static native String getWarningTemplate(int warningId);

  static native long getUuidHigh(long metadata, long codeOffset, long codeLength);

  static native long getUuidLow(long metadata, long codeOffset, long codeLength);

  public static long isIdentOrOperator(CharSequence input) {
    ensureInitialized();
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);
    return isIdentOrOperator(inputBuf);
  }

  public static ByteBuffer parseInputLazy(CharSequence input) {
    ensureInitialized();
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);
    return parseTreeLazy(inputBuf);
  }

  public static Tree parse(CharSequence input) {
    ensureInitialized();
    byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
    ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
    inputBuf.put(inputBytes);
    var serializedTree = parseTree(inputBuf);
    var base = getLastInputBase();
    var metadata = getMetadata();
    serializedTree.order(ByteOrder.LITTLE_ENDIAN);
    var message = new Message(serializedTree, input, base, metadata);
    return Tree.deserialize(message);
  }

  public static String getWarningMessage(Warning warning) {
    // `ensureInitialized` is unnecessary here because a `Warning` will only be constructed by
    // parsing something.
    return getWarningTemplate(warning.getId());
  }
}
