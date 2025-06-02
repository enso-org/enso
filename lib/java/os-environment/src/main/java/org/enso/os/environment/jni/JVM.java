package org.enso.os.environment.jni;

import java.io.File;
import java.io.IOException;
import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import org.enso.common.Platform;
import org.enso.persist.Persistance;
import org.graalvm.nativeimage.CurrentIsolate;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.function.CEntryPointLiteral;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.struct.SizeOf;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.WordFactory;

/** Represents a JVM inside of current process. */
public final class JVM {
  private final JNIBoot.JNICreateJavaVMPointer createJvmFn;
  private final String[] options;
  private JNI.JNIEnv env = WordFactory.nullPointer();

  JVM(JNIBoot.JNICreateJavaVMPointer factory, String[] options) {
    this.createJvmFn = factory;
    this.options = options;
  }

  /**
   * Create new JVM.Use {@link #env()} to obtain reference to JNI interface and make calls into the
   * JVM.
   *
   * @param javaHome path where the JDK is installed
   * @param options parameters to pass to the JVM
   * @return new instance of the JVM
   */
  public static JVM create(File javaHome, String... options) {
    var createJvmFn =
        switch (Platform.getOperatingSystem()) {
          case WINDOWS -> WindowsJVM.createImpl(javaHome);
          case LINUX, MACOS -> PosixJVM.createImpl(javaHome);
        };

    var jvmArgs = new ArrayList<String>();

    // java.home
    jvmArgs.add("-Djava.home=" + javaHome);

    var jvmOptions = System.getenv("JAVA_OPTS");
    if (jvmOptions != null) {
      for (var op : jvmOptions.split(" ")) {
        if (op.isEmpty()) {
          continue;
        }
        jvmArgs.add(op);
      }
    }
    jvmArgs.addAll(Arrays.asList(options));
    return new JVM(createJvmFn, jvmArgs.toArray(new String[0]));
  }

  static <R> R executeImpl(
      Persistance.Pool pool, Message<R> msg, Function<MemorySegment, Long> send) {
    try (var arena = Arena.ofConfined()) {
      var bytes = pool.write(msg, null);
      var memory = arena.allocate(Math.max(bytes.length, 4096));
      memory.copyFrom(MemorySegment.ofArray(bytes));
      long len = send.apply(memory);
      assert len >= 0;
      var reply = memory.asByteBuffer();
      reply.position(0);
      reply.limit((int) len);
      var result = pool.read(reply, null);
      return result.get(msg.replyType);
    } catch (IOException ex) {
      throw new IllegalStateException(ex);
    }
  }

  @CEntryPoint
  private static long acceptRequestFromHotSpotJvm(
      IsolateThread threadId, CCharPointer data, long size) throws Throwable {
    // TBD: recursive calls will need to find a proper channel
    var len = JVMPeer.handleWithChannel(null, data.rawValue(), size);
    return len;
  }

  static final CEntryPointLiteral<CFunctionPointer> CALLBACK_FN =
      CEntryPointLiteral.create(
          JVM.class,
          "acceptRequestFromHotSpotJvm",
          IsolateThread.class,
          CCharPointer.class,
          long.class);

  /** Channel connects two JVMs. */
  public static final class Channel {
    /** persistance pool associated with this channel object */
    private final Persistance.Pool pool;

    private final JNI.JNIEnv env;
    private final long isolate;
    private final long callbackFn;

    /* private */ Channel(Persistance.Pool pool, JNI.JNIEnv env) {
      this.pool = pool;
      this.env = env;
      this.isolate = -1;
      this.callbackFn = -1;
    }

    /* private */ Channel(Persistance.Pool pool, long isolate, long callbackFn) {
      if (ImageInfo.inImageCode()) {
        throw new IllegalStateException("Only usable in HotSpot");
      }
      this.pool = pool;
      this.env = null;
      this.isolate = isolate;
      this.callbackFn = callbackFn;
    }

    /**
     * <em>Executes a message</em> in the other JVM. The message is any subclass of {@link Message}
     * registered for persistance via {@link Persistable @Persistable} annotation into the {@link
     * Persistance.Pool pool associated with this JVM}. The result (which is of type {@code R}) also
     * has to be registered for serde.
     *
     * @param msg the message that gets serialized, transfered into the other JVM, deserialized on
     *     the other side and {@link Message#evaluate() evaluated} there
     * @param <R> the type of result we expect the message to return
     * @return the value gets computed via {@link Message#evaluate()} in the other JVM and then it
     *     gets serialized and transfered back to us. Deserialized and the value is then returned
     *     from this method
     */
    public final <R> R execute(Message<R> msg) {
      if (this.isolate == -1) {
        return executeImpl(pool, msg, (memory) -> toHotSpotMessage(env, memory));
      } else {
        var fnCallbackAddress = MemorySegment.ofAddress(callbackFn);
        var fnDescriptor =
            FunctionDescriptor.of(
                ValueLayout.JAVA_LONG,
                ValueLayout.ADDRESS,
                ValueLayout.ADDRESS,
                ValueLayout.JAVA_LONG);
        var fnHandle = Linker.nativeLinker().downcallHandle(fnCallbackAddress, fnDescriptor);
        return executeImpl(
            pool,
            msg,
            (seg) -> {
              Object res = -1L;
              try {
                var isoRef = MemorySegment.ofAddress(isolate);
                res = fnHandle.invoke(isoRef, seg, seg.byteSize());
              } catch (Throwable ex) {
                ex.printStackTrace();
              }
              return (long) res;
            });
      }
    }

    private static long toHotSpotMessage(JNI.JNIEnv e, MemorySegment segment) {
      var classNameWithSlashes = "org/enso/os/environment/jni/JVMPeer";
      var methodName = "handle";
      try (var classInC = CTypeConversion.toCString(classNameWithSlashes);
          var methodInC = CTypeConversion.toCString(methodName);
          var signatureInC = CTypeConversion.toCString("(JJJJ)J"); ) {
        var fn = e.getFunctions();
        var clazz = fn.getFindClass().call(e, classInC.get());
        assert clazz.isNonNull() : "Class not found " + classNameWithSlashes;
        var method = fn.getGetStaticMethodID().call(e, clazz, methodInC.get(), signatureInC.get());
        assert method.isNonNull() : "method not found in " + classNameWithSlashes;
        var address = segment.address();
        assert address > 0 : "We need an address";
        var arg = StackValue.get(4, JNI.JValue.class);
        arg.addressOf(0).setLong(CurrentIsolate.getCurrentThread().rawValue());
        arg.addressOf(1).setLong(CALLBACK_FN.getFunctionPointer().rawValue());
        arg.addressOf(2).setLong(address);
        arg.addressOf(3).setLong(segment.byteSize());
        var replySize = fn.getCallStaticLongMethodA().call(e, clazz, method, arg);
        return replySize;
      }
    }
  }

  /**
   * Subclasses of message denote a computational task to be performed in the "other JVM".
   *
   * @param <R> type of the return value
   */
  public abstract static class Message<R> {
    private final Class<R> replyType;

    /**
     * Constructor for subclasses. Use it as {@code super(Integer.class)} to specify the reply type
     * of the exception which is then returned from the {@link
     * #execute(org.enso.os.environment.jni.JVM.Message)} method.
     *
     * @param replyType the type of the reply
     */
    protected Message(Class<R> replyType) {
      this.replyType = replyType;
    }

    /**
     * Evaluates the exception. Invoked in the other JVM.
     *
     * @param channel allows sending messages to the other JVM
     * @return the result of the evaluation or {@code null}
     * @throws Throwable the computation may yield exceptions or errors which are then transferred
     *     back to the callee JVM
     */
    protected abstract R evaluate(Channel channel) throws Throwable;
  }

  /**
   * Executes main method of provided class
   *
   * @param classNameWithSlashes class (with `/` as separators) to search main method in
   * @param args arguments to pass to the main method
   */
  public final void executeMain(String classNameWithSlashes, String... args) {
    var msg = new JVMPeer.ExecuteMainClass(classNameWithSlashes, List.of(args));
    var channel = new Channel(JVMPeer.POOL, env());
    channel.execute(msg);
  }

  /**
   * Initialize or just obtain environment associated with this JVM.
   *
   * @return JNI environment to make calls into the JVM
   */
  final JNI.JNIEnv env() {
    if (env.isNull()) {
      env = initializeEnv();
    }
    return env;
  }

  private synchronized JNI.JNIEnv initializeEnv() {
    var jvmArgs = StackValue.get(JNIBoot.Args.class);
    var optionsCount = options.length;
    jvmArgs.nOptions(optionsCount);
    var sizeOfOption = SizeOf.get(JNIBoot.Option.class);
    JNIBoot.Option jvmOpts = UnmanagedMemory.calloc(optionsCount * sizeOfOption);
    var holder = new CTypeConversion.CCharPointerHolder[optionsCount];
    for (var i = 0; i < optionsCount; i++) {
      holder[i] = CTypeConversion.toCString(options[i]);
      var nth = jvmOpts.addressOf(i);
      nth.setOptionString(holder[i].get());
      nth.setExtraInfo(WordFactory.nullPointer());
    }
    jvmArgs.options(jvmOpts);
    jvmArgs.version(JNI.JNI_VERSION_10());
    jvmArgs.ignoreUnrecognized(false);

    var jvmPtr = StackValue.get(JNI.JavaVMPointer.class);
    var envPtr = StackValue.get(JNI.JNIEnvPointer.class);

    int res = createJvmFn.call(jvmPtr, envPtr, jvmArgs);
    if (res != 0) {
      throw new AssertionError("Error creating JVM: " + res);
    }

    for (var i = 0; i < optionsCount; i++) {
      holder[i].close();
    }
    UnmanagedMemory.free(jvmOpts);

    return envPtr.readJNIEnv();
  }
}
