package org.enso.os.environment.jni;

import java.io.IOException;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.lang.invoke.MethodHandle;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import org.enso.persist.Persistance;
import org.graalvm.nativeimage.CurrentIsolate;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.function.CEntryPointLiteral;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;
import org.graalvm.word.WordFactory;

/** Channel connects two {@link JVM} instances. */
public final class Channel implements AutoCloseable {
  /**
   * @GuardedBy("Channel.class")
   */
  private static final Map<Long, Channel> ID_TO_CHANNEL = new HashMap<>();

  /**
   * @GuardedBy("Channel.class")
   */
  private static long idCounter = 1;

  /** persistance pool associated with this channel object */
  private final Persistance.Pool pool;

  private final long id;
  private final JNI.JNIEnv env;
  private final long isolate;
  private final MethodHandle callbackFn;
  private final JNI.JClass channelClass;
  private final JNI.JMethodID channelHandle;

  /** The SubstrateVM side of a channel. */
  private Channel(
      long id,
      Persistance.Pool pool,
      JNI.JNIEnv env,
      JNI.JClass handleClass,
      JNI.JMethodID handleFn) {
    this.id = id;
    this.pool = pool;
    this.env = env;
    this.isolate = -1;
    this.callbackFn = null;
    this.channelClass = handleClass;
    this.channelHandle = handleFn;
  }

  /** The HotSpot JVM side of a channel. */
  private Channel(long id, Persistance.Pool pool, long isolate, long callbackFn) {
    if (ImageInfo.inImageCode()) {
      throw new IllegalStateException("Only usable in HotSpot");
    }
    this.id = id;
    this.pool = pool;
    this.isolate = isolate;
    this.env = null;
    this.channelClass = null;
    this.channelHandle = null;

    var fnCallbackAddress = MemorySegment.ofAddress(callbackFn);
    var fnDescriptor =
        FunctionDescriptor.of(
            ValueLayout.JAVA_LONG,
            ValueLayout.ADDRESS,
            ValueLayout.JAVA_LONG,
            ValueLayout.ADDRESS,
            ValueLayout.JAVA_LONG);
    this.callbackFn = Linker.nativeLinker().downcallHandle(fnCallbackAddress, fnDescriptor);
  }

  /**
   * Factory method to initialize the Channel in the SubstrateVM.
   *
   * @param jvm instance of HotSpot JVM to connect to
   * @param poolClass the class which has public default constructor and can supply an instance of
   *     persistance pool to use for communication
   * @return channel for sending messages to the HotSpot JVM
   */
  public static synchronized Channel create( //
      JVM jvm, //
      Class<? extends Supplier<Persistance.Pool>> poolClass //
      ) {
    var id = idCounter++;
    var e = jvm.env();
    var classNameWithSlashes = Channel.class.getName().replace('.', '/');
    try (var classInC = CTypeConversion.toCString(classNameWithSlashes);
        var poolClassInC = CTypeConversion.toCString(poolClass.getName());
        var createInC = CTypeConversion.toCString("createJvmPeerChannel");
        var createSigInC = CTypeConversion.toCString("(JJJLjava/lang/String;)Z"); //
        var handleInC = CTypeConversion.toCString("handleJvmMessage");
        var handleSigInC = CTypeConversion.toCString("(JJJ)J"); //
        ) {
      var fn = e.getFunctions();
      var channelClass = fn.getFindClass().call(e, classInC.get());
      assert channelClass.isNonNull() : "Class not found " + classNameWithSlashes;
      var createMethod =
          fn.getGetStaticMethodID().call(e, channelClass, createInC.get(), createSigInC.get());
      assert createMethod.isNonNull() : "method not found in " + classNameWithSlashes;
      var poolClassInHotSpot = fn.getNewStringUTF().call(e, poolClassInC.get());
      var handleMethod =
          fn.getGetStaticMethodID().call(e, channelClass, handleInC.get(), handleSigInC.get());

      var pool = poolClass.getConstructor().newInstance().get();
      var channel = new Channel(id, pool, e, channelClass, handleMethod);

      var arg = StackValue.get(4, JNI.JValue.class);
      arg.addressOf(0).setLong(id);
      arg.addressOf(1).setLong(CurrentIsolate.getCurrentThread().rawValue());
      arg.addressOf(2).setLong(CALLBACK_FN.getFunctionPointer().rawValue());
      arg.addressOf(3).setJObject(poolClassInHotSpot);
      var replyOk = fn.getCallStaticBooleanMethodA().call(e, channelClass, createMethod, arg);
      channel.checkForException(e);
      assert replyOk : "Failed to create peer in HotSpot JVM";

      ID_TO_CHANNEL.put(id, channel);
      return channel;
    } catch (ReflectiveOperationException ex) {
      throw new IllegalStateException(ex);
    }
  }

  /** Allocates new channel with given ID in the HotSpot VM. Called via JNI/foreign interface. */
  private static boolean createJvmPeerChannel(
      long id, long threadId, long callbackFn, String poolClassName) throws Throwable {
    @SuppressWarnings("unchecked")
    var factory =
        (Supplier<Persistance.Pool>) Class.forName(poolClassName).getConstructor().newInstance();
    var pool = factory.get();
    var channel = new Channel(id, pool, threadId, callbackFn);
    var prev = ID_TO_CHANNEL.put(id, channel);
    return prev == null;
  }

  /**
   * <em>Executes a message</em> in the other JVM. The message is any subclass of {@link Function}
   * registered for persistance via {@link Persistable @Persistable} annotation into the {@link
   * Persistance.Pool pool associated with this JVM}. The result (which is of type {@code R}) also
   * has to be registered for serde.
   *
   * <p>
   *
   * @param resultType class with the type of {@code R} to use for deserialization
   * @param msg the message that gets serialized, transferred into the other JVM, deserialized on
   *     the other side and {@link Message#evaluate() evaluated} there
   * @param <R> the type of result we expect the message to return
   * @return the value gets computed via {@link Message#evaluate()} in the other JVM and then it
   *     gets serialized and transferred back to us. Deserialized and the value is then returned
   *     from this method
   */
  public final <R> R execute(Class<R> resultType, Function<Channel, R> msg) {
    return executeImpl(pool, resultType, msg);
  }

  private static final CEntryPointLiteral<CFunctionPointer> CALLBACK_FN =
      CEntryPointLiteral.create(
          Channel.class,
          "acceptRequestFromHotSpotJvm",
          IsolateThread.class,
          long.class,
          CCharPointer.class,
          long.class);

  @CEntryPoint
  private static long acceptRequestFromHotSpotJvm(
      IsolateThread threadId, long id, CCharPointer data, long size) throws Throwable {

    var channel = ID_TO_CHANNEL.get(id);
    assert channel != null : "There must be a channel " + id + " but " + ID_TO_CHANNEL;
    try {
      var buf = asNativeByteBuffer(data, size);
      var len = handleWithChannel(channel, buf);
      return len;
    } catch (Throwable ex) {
      channel.printStackTrace(ex, true);
      var bytes = ex.getMessage() == null ? new byte[0] : ex.getMessage().getBytes();
      var buf = asNativeByteBuffer(data, size);
      buf.putInt(bytes.length);
      buf.put(bytes);
      return -2L;
    }
  }

  private static long handleWithChannel(Channel channel, ByteBuffer buf) throws Throwable {
    var ref = channel.pool.read(buf, null);
    var msg = ref.get(Function.class);
    @SuppressWarnings("unchecked")
    var res = msg.apply(channel);
    var bytes = Persistables.POOL.write(res, null);
    buf.put(0, bytes);
    return bytes.length;
  }

  private long toHotSpotMessage(long address, long size) {
    var fn = env.getFunctions();
    assert address > 0 : "We need an address";
    var arg = StackValue.get(3, JNI.JValue.class);
    arg.addressOf(0).setLong(id);
    arg.addressOf(1).setLong(address);
    arg.addressOf(2).setLong(size);
    var replySize = fn.getCallStaticLongMethodA().call(env, channelClass, channelHandle, arg);
    checkForException(env);
    return replySize;
  }

  private long toSubstrateMessage(MemorySegment seg) {
    try {
      var isoRef = MemorySegment.ofAddress(isolate);
      var res = callbackFn.invoke(isoRef, id, seg, seg.byteSize());
      return (long) res;
    } catch (Throwable ex) {
      printStackTrace(ex, false);
      return -1L;
    }
  }

  private void checkForException(JNI.JNIEnv e) {
    var fn = e.getFunctions();
    if (fn.getExceptionCheck().call(e)) {
      var throwable = fn.getExceptionOccurred().call(e);
      assert throwable.isNonNull() : "There must be a throwable";
      if (printStackTrace(null, true)) {
        fn.getExceptionDescribe().call(e);
      }
      fn.getExceptionClear().call(e);
      try (var throwableInC = CTypeConversion.toCString("java/lang/Throwable");
          var messageInC = CTypeConversion.toCString("getMessage");
          var messageSigInC = CTypeConversion.toCString("()Ljava/lang/String;")) {
        var throwableClass = fn.getFindClass().call(e, throwableInC.get());
        var messageMethod =
            fn.getGetMethodID().call(e, throwableClass, messageInC.get(), messageSigInC.get());
        var args = StackValue.get(1, JNI.JValue.class);
        var msg = (JNI.JString) fn.getCallObjectMethodA().call(e, throwable, messageMethod, args);
        args.addressOf(0).setBoolean(false);
        var cStr = fn.getGetStringUTFChars().call(e, msg, args);
        var javaMsg = CTypeConversion.toJavaString(cStr);
        fn.getReleaseStringUTFChars().call(e, msg, cStr);
        throw new IllegalStateException(javaMsg);
      }
    }
  }

  private <R> R executeImpl( //
      Persistance.Pool pool, //
      Class<R> replyType, //
      Function<Channel, R> msg //
      ) {
    var address = 0L;
    try {
      var bytes = pool.write(msg, null);
      var size = Math.max(bytes.length, 4096);
      long len;
      ByteBuffer buffer;
      if (ImageInfo.inImageRuntimeCode()) {
        var memory = UnmanagedMemory.malloc(size);
        buffer = asNativeByteBuffer(memory, size);
        buffer.put(0, bytes);
        address = memory.rawValue();
        len = toHotSpotMessage(address, size);
      } else {
        buffer = ByteBuffer.allocateDirect(size);
        var memory = MemorySegment.ofBuffer(buffer);
        memory.copyFrom(MemorySegment.ofArray(bytes));
        address = memory.address();
        len = toSubstrateMessage(memory);
      }
      if (len == -2) {
        // signals exception
        buffer.position(0);
        var msgLen = buffer.getInt();
        var msgBytes = new byte[msgLen];
        buffer.get(msgBytes);
        var exceptionMessage = new String(msgBytes);
        throw new IllegalStateException(exceptionMessage);
      }
      assert len >= 0;
      buffer.position(0);
      buffer.limit((int) len);
      var result = pool.read(buffer, null);
      return result.get(replyType);
    } catch (IOException ex) {
      throw new IllegalStateException(ex);
    } finally {
      if (ImageInfo.inImageRuntimeCode()) {
        UnmanagedMemory.free(WordFactory.pointer(address));
      }
    }
  }

  private static ByteBuffer asNativeByteBuffer(PointerBase memory, long size) {
    var bufferSize = Math.toIntExact(size);
    return CTypeConversion.asByteBuffer(memory, bufferSize).order(ByteOrder.BIG_ENDIAN);
  }

  private static long handleJvmMessage(long id, long address, long size) throws Throwable {
    var channel = ID_TO_CHANNEL.get(id);
    var seg = MemorySegment.ofAddress(address).reinterpret(size);
    return handleWithChannel(channel, seg.asByteBuffer());
  }

  @Override
  public void close() throws Exception {
    ID_TO_CHANNEL.remove(id, this);
    // TBD remove on the peer as well
  }

  /**
   * @param ex exception to print stack trace for or {@code null}
   * @param userCode is the exception from user code or is it unexpected
   * @return {@code true} if the exception was printed and further details should be printed
   */
  private boolean printStackTrace(Throwable ex, boolean userCode) {
    if (!userCode) {
      if (ex != null) {
        ex.printStackTrace();
      }
      return true;
    }
    return false;
  }
}
