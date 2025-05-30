package org.enso.os.environment.jni;

import java.io.File;
import java.io.IOException;
import java.lang.foreign.Arena;
import java.lang.foreign.MemorySegment;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.enso.common.Platform;
import org.enso.persist.Persistance;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.struct.SizeOf;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.WordFactory;

/** Represents a JVM inside of current process. */
public final class JVM {
  private final JNIBoot.JNICreateJavaVMPointer createJvmFn;
  private final String[] options;
  private JNI.JNIEnv env = WordFactory.nullPointer();

  /** persistance pool associated with this JVM object */
  private final Persistance.Pool pool;

  JVM(Persistance.Pool pool, JNIBoot.JNICreateJavaVMPointer factory, String[] options) {
    this.pool = pool;
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
    return new JVM(JVMPeer.POOL, createJvmFn, jvmArgs.toArray(new String[0]));
  }

  /**
   * <em>Executes a message</em> in the other JVM. The message is any subclass of {@link Message}
   * registered for persistance via {@link Persistable @Persistable} annotation into the {@link
   * Persistance.Pool pool associated with this JVM}. The result (which is of type {@code R}) also
   * has to be registered for serde.
   *
   * @param msg the message that gets serialized, transfered into the other JVM, deserialized on the
   *     other side and {@link Message#evaluate() evaluated} there
   * @param <R> the type of result we expect the message to return
   * @return the value gets computed via {@link Message#evaluate()} in the other JVM and then it
   *     gets serialized and transfered back to us. Deserialized and the value is then returned from
   *     this method
   */
  public final <R> R execute(Message<R> msg) {
    try (var arena = Arena.ofConfined()) {
      var bytes = pool.write(msg, null);
      var memory = arena.allocate(Math.max(bytes.length, 4096));
      memory.copyFrom(MemorySegment.ofArray(bytes));
      long len = executeMessageBytes("org/enso/os/environment/jni/JVMPeer", "handle", memory);
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

  private long executeMessageBytes(
      String classNameWithSlashes, String method, MemorySegment segment) {
    var e = env();
    try (var className = CTypeConversion.toCString(classNameWithSlashes);
        var methodName = CTypeConversion.toCString(method);
        var methodSig = CTypeConversion.toCString("(JJ)J"); ) {
      var fn = e.getFunctions();
      var clazz = fn.getFindClass().call(e, className.get());
      assert clazz.isNonNull() : "Class not found " + classNameWithSlashes;
      var mainMethod = fn.getGetStaticMethodID().call(e, clazz, methodName.get(), methodSig.get());
      assert mainMethod.isNonNull() : "method not found in " + classNameWithSlashes;
      var address = segment.address();
      assert address > 0 : "We need an address";
      var arg = StackValue.get(2, JNI.JValue.class);
      arg.addressOf(0).setLong(address);
      arg.addressOf(1).setLong(segment.byteSize());
      var replySize = fn.getCallStaticLongMethodA().call(e, clazz, mainMethod, arg);
      return replySize;
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
     * @return the result of the evaluation or {@code null}
     * @throws Throwable the computation may yield exceptions or errors which are then transferred
     *     back to the callee JVM
     */
    protected abstract R evaluate() throws Throwable;
  }

  /**
   * Executes main method of provided class
   *
   * @param classNameWithSlashes class (with `/` as separators) to search main method in
   * @param args arguments to pass to the main method
   */
  public final void executeMain(String classNameWithSlashes, String... args) {
    var msg = new JVMPeer.ExecuteMainClass(classNameWithSlashes, List.of(args));
    execute(msg);
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
