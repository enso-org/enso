package org.enso.syntax2;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.BufferUnderflowException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import org.graalvm.nativeimage.ImageInfo;
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

  private static Worker getWorker() {
    finalizationManager.runPendingFinalizers();
    return threadWorker.get();
  }

  private static Worker createWorker() {
    var worker = Worker.create();
    if (!ImageInfo.inImageBuildtimeCode()) {
      // At build-time, we eagerly free parser buffers; runtime should start out with an empty
      // `finalizationManager`.
      finalizationManager.attachFinalizer(worker, worker.finalizer());
      threadWorker.set(worker);
    }
    return worker;
  }

  private static final ThreadLocal<Worker> threadWorker =
      ThreadLocal.withInitial(Parser::createWorker);

  public static void freeAll() {
    for (var finalizer : finalizationManager.getRegisteredFinalizers()) finalizer.run();
  }

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

    private final AtomicLong state = new AtomicLong(0);

    private Worker() {}

    private static class Finalizer implements Runnable {
      private final AtomicLong state;

      private Finalizer(AtomicLong state) {
        this.state = state;
      }

      @Override
      public void run() {
        freeState(state.getAndSet(0));
      }
    }

    Runnable finalizer() {
      return new Finalizer(state);
    }

    static Worker create() {
      initializeLibraries();
      return new Worker();
    }

    private <T> T withState(Function<Long, T> stateConsumer) {
      // Take the state for the duration of the operation so that it can't be freed by another
      // thread.
      var privateState = state.getAndSet(0);
      if (privateState == 0) privateState = allocState();
      var result = stateConsumer.apply(privateState);
      if (ImageInfo.inImageBuildtimeCode()) {
        // At build-time, eagerly free buffers. We don't want them included in the heap snapshot!
        freeState(privateState);
      } else {
        // We don't need to check the value before setting here: A state may be freed by another
        // thread, but is only allocated by its associated `Worker`, so after taking it above, the
        // shared value remains 0 until we restore it.
        state.set(privateState);
      }
      return result;
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
      return withState(state -> parseTreeLazy(state, inputBuf));
    }

    Tree parse(CharSequence input) {
      byte[] inputBytes = input.toString().getBytes(StandardCharsets.UTF_8);
      ByteBuffer inputBuf = ByteBuffer.allocateDirect(inputBytes.length);
      inputBuf.put(inputBytes);
      return withState(
          state -> {
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
          });
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
