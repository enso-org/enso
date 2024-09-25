package org.enso.syntax2;

import java.io.File;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.net.URISyntaxException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.UUID;
import org.slf4j.LoggerFactory;

public final class Parser {
  private Parser() {}

  public static String getWarningMessage(Warning warning) {
    return getWarningTemplate(warning.getId());
  }

  public static long isIdentOrOperator(CharSequence input) {
    return getWorker().isIdentOrOperator(input);
  }

  public static ByteBuffer parseInputLazy(CharSequence input) {
    return getWorker().parseInputLazy(input);
  }

  public static Tree parse(CharSequence input) {
    return getWorker().parse(input);
  }

  public static UUID getUuid(long metadata, long nodeOffset, long nodeLength) {
    long high = getUuidHigh(metadata, nodeOffset, nodeLength);
    long low = getUuidLow(metadata, nodeOffset, nodeLength);
    if (high == 0 && low == 0) {
      // The native interface uses the Nil UUID value as a marker to indicate that no UUID was
      // attached.
      // The Nil UUID will never collide with a real UUID generated by any scheme.
      return null;
    }
    return new UUID(high, low);
  }

  /* Worker-thread state */

  private static final FinalizationManager finalizationManager = new FinalizationManager();
  private static final Thread finalizationThread = new Thread(finalizationManager.createRunner());

  private static Worker getWorker() {
    var threadWorker = worker.get();
    if (threadWorker != null) {
      var reusableWorker = threadWorker.get();
      if (reusableWorker != null) return reusableWorker;
    }
    var newWorker = Worker.create();
    finalizationManager.attachFinalizer(newWorker, newWorker.finalizer());
    worker.set(new SoftReference<>(newWorker));
    return newWorker;
  }

  private static final ThreadLocal<Reference<Worker>> worker = new ThreadLocal<>();

  private static class Worker {
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

    private final long state;

    private Worker() {
      state = allocState();
    }

    /**
     * @return A function that can be called to free the associated resources. The function *must
     *     not* be called more than once.
     */
    Runnable finalizer() {
      return () -> freeState(state);
    }

    static Worker create() {
      initializeLibraries();
      return new Worker();
    }

    long isIdentOrOperator(CharSequence input) {
      byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
      ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
      inputBuf.put(inputBytes);

      return Parser.isIdentOrOperator(inputBuf);
    }

    ByteBuffer parseInputLazy(CharSequence input) {
      byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
      ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
      inputBuf.put(inputBytes);
      return parseTreeLazy(state, inputBuf);
    }

    Tree parse(CharSequence input) {
      byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
      ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
      inputBuf.put(inputBytes);
      var serializedTree = parseTree(state, inputBuf);
      var base = getLastInputBase(state);
      var metadata = getMetadata(state);
      serializedTree.order(ByteOrder.LITTLE_ENDIAN);
      var message = new Message(serializedTree, input, base, metadata);
      try {
        return Tree.deserialize(message);
      } catch (BufferUnderflowException | IllegalArgumentException e) {
        LoggerFactory.getLogger(this.getClass())
            .error("Unrecoverable parser failure for: {}", input, e);
        throw e;
      }
    }
  }

  /* JNI declarations */

  private static native long allocState();

  private static native void freeState(long state);

  private static native ByteBuffer parseTree(long state, ByteBuffer input);

  private static native ByteBuffer parseTreeLazy(long state, ByteBuffer input);

  private static native long isIdentOrOperator(ByteBuffer input);

  private static native long getLastInputBase(long state);

  private static native long getMetadata(long state);

  private static native String getWarningTemplate(int warningId);

  private static native long getUuidHigh(long metadata, long codeOffset, long codeLength);

  private static native long getUuidLow(long metadata, long codeOffset, long codeLength);
}
