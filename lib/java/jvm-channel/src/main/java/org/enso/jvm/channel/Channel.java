package org.enso.jvm.channel;

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
import org.enso.persist.Persistance;
import org.graalvm.nativeimage.CurrentIsolate;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.UnmanagedMemory;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.function.CEntryPointLiteral;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.function.InvokeCFunctionPointer;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import org.graalvm.word.PointerBase;
import org.graalvm.word.WordFactory;

/**
 * Channel connects two {@link JVM} instances. A "channel" creates two identical instances of the
 * {@code Channel} on both sides of the "channel" - e.g. in each of the JVMs. The instances are
 * initialized with the same {@link Persistance.Pool}, so they both understand the same messages.
 *
 * @param <Data> internal data of the channel
 */
public final class Channel<Data extends Channel.Config> implements AutoCloseable {
  private static final byte TYPE_MASTER_ISOLATE = -1;
  private static final byte TYPE_SLAVE_ISOLATE = -2;
  private static final byte TYPE_MOCK_MASTER = -3;
  private static final byte TYPE_MOCK_SLAVE = -4;

  private static final long RET_CODE_EXCEPTION = -2;
  private static final long RET_CODE_OVERFLOW = -3;

  /**
   * @GuardedBy("Channel.class")
   */
  private static final Map<Long, Channel> ID_TO_CHANNEL = new HashMap<>();

  /**
   * @GuardedBy("Channel.class")
   */
  private static long idCounter = 1;

  /**
   * prevent the buffer from being GCed too soon. Keep it until next call. By default the buffers
   * are allocated by callers. After the call is made the caller then deallocates the buffer.
   *
   * <p>However, when there is an overflow, the buffer must be allocated by the callee. We need the
   * buffer to survive "a while" before the caller reads it. For now, the callee stores the buffer
   * here. The value gets cleared on next call.
   */
  private static ThreadLocal<ByteBuffer> keepLastOverflowBuffer = new ThreadLocal<>();

  /** data associated with the channel */
  private final Data data;

  /** persistance pool associated with this channel object */
  private final Persistance.Pool pool;

  private final long id;
  private final JVM jvm;
  private final byte type;
  private final Object callbackFn;
  private final JNI.JClass channelClass;
  private final JNI.JMethodID channelHandle;
  private final Channel<Data> otherMockChannel;
  private final ThreadLocal<Long> otherIsolateThread = new ThreadLocal<>();

  /** The SubstrateVM side of a channel. */
  private Channel(long id, Data data, JVM jvm, JNI.JClass handleClass, JNI.JMethodID handleFn) {
    this.id = id;
    this.data = data;
    this.jvm = jvm;
    this.type = TYPE_MASTER_ISOLATE;
    this.callbackFn = null;
    this.channelClass = handleClass;
    this.channelHandle = handleFn;
    this.otherMockChannel = null;
    this.pool = data.createPool(this);
  }

  /**
   * The other JVM side of a channel. This side can be executed either in HotSpot JVM or also loaded
   * from an SVM compiled dynamic library.
   */
  private Channel(long id, Data data, long callbackFn) {
    this.id = id;
    this.data = data;
    this.type = TYPE_SLAVE_ISOLATE;
    this.otherMockChannel = null;

    if (ImageInfo.inImageRuntimeCode()) {
      this.jvm = null;
      this.channelClass = WordFactory.nullPointer();
      this.channelHandle = WordFactory.nullPointer();
      this.callbackFn = callbackFn;
    } else {
      this.jvm = null;
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
    this.pool = data.createPool(this);
  }

  /**
   * Mock constructor. Creates a channel that simulates sending of the messages inside of the same
   * JVM. Useful for testing.
   */
  private Channel(byte type, Data myData, Channel<Data> otherOrNull, Data otherData, long id) {
    if (ImageInfo.inImageCode()) {
      throw new IllegalStateException("Only usable in HotSpot");
    }
    this.id = id;
    this.data = myData;
    this.type = type;
    this.callbackFn = null;
    this.jvm = null;
    this.channelClass = null;
    this.channelHandle = null;
    this.otherMockChannel =
        otherOrNull != null
            ? otherOrNull // use other channel when provided
            : // otherwise allocate new and pass this reference to it
            new Channel<>(TYPE_MOCK_SLAVE, otherData, this, null, id);
    this.pool = data.createPool(this);
  }

  /**
   * Factory method to initialize the Channel in the SubstrateVM.
   *
   * @param <D> type of internal data as well as provider of the pool
   * @param jvm instance of HotSpot JVM to connect to (can be {@code null} to create a mock channel
   *     inside of a single JVM)
   * @param configClass the class which has public default constructor and can supply an instance of
   *     persistance pool to use for communication
   * @return channel for sending messages to the HotSpot JVM
   */
  public static synchronized <D extends Config> Channel<D> create(
      JVM jvm, Class<? extends D> configClass) {
    var config = newInstance(configClass);
    var id = idCounter++;
    if (jvm == null) {
      var otherData = newInstance(configClass);
      return new Channel<>(TYPE_MOCK_MASTER, config, null, otherData, id);
    }

    if (!ImageInfo.inImageCode()) {
      throw new IllegalStateException("Only usable from SubstrateVM");
    }
    var e = jvm.env();
    var classNameWithSlashes = Channel.class.getName().replace('.', '/');
    try (var classInC = CTypeConversion.toCString(classNameWithSlashes);
        var poolClassInC = CTypeConversion.toCString(configClass.getName());
        var createInC = CTypeConversion.toCString("createJvmPeerChannel");
        var createSigInC = CTypeConversion.toCString("(JJJLjava/lang/String;)Z"); //
        var handleInC = CTypeConversion.toCString("handleJvmMessage");
        var handleSigInC = CTypeConversion.toCString("(JJJJ)J"); //
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

      var channel = new Channel<>(id, config, jvm, channelClass, handleMethod);

      var arg = StackValue.get(4, JNI.JValue.class);
      arg.addressOf(0).setLong(id);
      arg.addressOf(1).setLong(CurrentIsolate.getCurrentThread().rawValue());
      arg.addressOf(2).setLong(CALLBACK_FN.getFunctionPointer().rawValue());
      arg.addressOf(3).setJObject(poolClassInHotSpot);
      var replyOk = fn.getCallStaticBooleanMethodA().call(e, channelClass, createMethod, arg);
      channel.checkForException(e, false);
      assert replyOk : "Failed to create peer in HotSpot JVM";

      ID_TO_CHANNEL.put(id, channel);
      return channel;
    }
  }

  /**
   * Getter for data associated with the channel. Each instance of {@code Channel} on both sides of
   * the "channel" gets different instance of {@code Data}. The data may be used in the functions
   * that implement the logic in {@link #execute} message processing.
   *
   * @return data associated with this channel
   * @see #execute
   */
  public final Data getConfig() {
    return data;
  }

  /**
   * Master channel check. One instance of the {@code Channel} on the initializing side is marked as
   * master. The other one is slave.
   *
   * @return is master
   */
  public final boolean isMaster() {
    return type == TYPE_MASTER_ISOLATE || type == TYPE_MOCK_MASTER;
  }

  final boolean isDirect() {
    return type == TYPE_MOCK_MASTER || type == TYPE_MOCK_SLAVE;
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
  @SuppressWarnings("unchecked")
  public final <C, R extends C> R execute(
      Class<C> resultType, Function<? super Channel<Data>, R> msg) {
    var r = (R) executeImpl(pool, resultType, (Function) msg);
    return r;
  }

  //
  // implementation
  //

  private static <T> T newInstance(Class<T> poolClass) {
    try {
      return poolClass.getConstructor().newInstance();
    } catch (ReflectiveOperationException ex) {
      throw new IllegalArgumentException(ex);
    }
  }

  /** Allocates new channel with given ID in the HotSpot VM. Called via JNI/foreign interface. */
  @SuppressWarnings("unchecked")
  private static boolean createJvmPeerChannel(
      long id, long threadId, long callbackFn, String poolClassName) throws Throwable {
    var configClass = Class.forName(poolClassName);
    var data = (Config) newInstance(configClass);
    var channel = new Channel<>(id, data, callbackFn);
    var prev = ID_TO_CHANNEL.put(id, channel);
    return prev == null;
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
      return RET_CODE_EXCEPTION;
    }
  }

  private static long handleWithChannel(Channel channel, ByteBuffer buf) throws IOException {
    // clean any previous overflow buffer
    keepLastOverflowBuffer.set(null);

    var ref = channel.pool.read(buf);
    var msg = ref.get(Function.class);
    @SuppressWarnings("unchecked")
    var res = msg.apply(channel);
    var bytes = channel.pool.write(res);
    if (bytes.length <= buf.limit()) {
      buf.put(0, bytes);
      return bytes.length;
    } else {
      var ownBuffer = ByteBuffer.allocateDirect(bytes.length);
      ownBuffer.put(0, bytes);
      var ownSeg = MemorySegment.ofBuffer(ownBuffer);
      buf.position(0); // at begining put
      buf.limit(16); // two longs
      buf.putLong(bytes.length);
      buf.putLong(ownSeg.address());
      keepLastOverflowBuffer.set(buf);
      return RET_CODE_OVERFLOW;
    }
  }

  private long toHotSpotMessage(long address, long size) {
    var env = jvm.env();
    var fn = env.getFunctions();
    assert address > 0 : "We need an address";
    var arg = StackValue.get(4, JNI.JValue.class);
    arg.addressOf(0).setLong(CurrentIsolate.getCurrentThread().rawValue());
    arg.addressOf(1).setLong(id);
    arg.addressOf(2).setLong(address);
    arg.addressOf(3).setLong(size);
    var replySize = fn.getCallStaticLongMethodA().call(env, channelClass, channelHandle, arg);
    checkForException(env, true);
    return replySize;
  }

  interface CallbackFn extends CFunctionPointer {
    @InvokeCFunctionPointer
    long invoke(long isoRef, long id, long seg, long size);
  }

  private long toSubstrateMessage(MemorySegment seg) {
    try {
      Long isolate = otherIsolateThread.get();
      if (isolate == null) {
        throw new IllegalStateException("There is no associated other isolate thread!");
      }
      var isoRef = MemorySegment.ofAddress(isolate);
      if (callbackFn instanceof MethodHandle handle) {
        var res = handle.invoke(isoRef, id, seg, seg.byteSize());
        return (long) res;
      } else {
        CallbackFn fn = WordFactory.pointer((Long) callbackFn);
        var res = fn.invoke(isolate, id, seg.address(), seg.byteSize());
        return res;
      }
    } catch (Throwable ex) {
      printStackTrace(ex, false);
      return -1L;
    }
  }

  private long toDirectMessage(ByteBuffer buf) throws IOException {
    assert otherMockChannel != null;
    buf.position(0);
    var len = handleWithChannel(otherMockChannel, buf);
    buf.position(0);
    return len;
  }

  private void checkForException(JNI.JNIEnv e, boolean userCode) {
    var fn = e.getFunctions();
    var hasException = fn.getExceptionCheck().call(e);
    if (hasException) {
      var throwable = fn.getExceptionOccurred().call(e);
      assert throwable.isNonNull() : "There must be a throwable";
      if (printStackTrace(null, userCode)) {
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

  private <R> R executeImpl(
      Persistance.Pool pool,
      Class<R> replyType,
      Function<Channel<? extends Data>, ? extends R> msg) {
    var address = 0L;
    var useMalloc = isMaster() && !isDirect();
    try {
      var bytes = pool.write(msg);
      var size = Math.max(bytes.length, 4096);
      long len;
      ByteBuffer buffer;
      if (useMalloc) {
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
        len = isDirect() ? toDirectMessage(buffer) : toSubstrateMessage(memory);
      }
      if (len == RET_CODE_EXCEPTION) {
        // signals exception
        buffer.position(0);
        var msgLen = buffer.getInt();
        var msgBytes = new byte[msgLen];
        buffer.get(msgBytes);
        var exceptionMessage = new String(msgBytes);
        throw new IllegalStateException(exceptionMessage);
      }
      if (len == RET_CODE_OVERFLOW) {
        buffer.position(0);
        // read length
        len = buffer.getLong();
        // read address
        var addr = buffer.getLong();
        if (useMalloc) {
          var overflowPtr = WordFactory.pointer(addr);
          buffer =
              CTypeConversion.asByteBuffer(overflowPtr, Math.toIntExact(len))
                  .order(ByteOrder.BIG_ENDIAN);
        } else {
          var overflowSegment = MemorySegment.ofAddress(addr).reinterpret(len);
          buffer = overflowSegment.asByteBuffer();
        }
      }
      assert len >= 0;
      buffer.position(0);
      buffer.limit(Math.toIntExact(len));
      var result = pool.read(buffer);
      return result.get(replyType);
    } catch (IOException ex) {
      throw new IllegalStateException(ex);
    } finally {
      if (useMalloc) {
        UnmanagedMemory.free(WordFactory.pointer(address));
      }
    }
  }

  private static ByteBuffer asNativeByteBuffer(PointerBase memory, long size) {
    var bufferSize = Math.toIntExact(size);
    return CTypeConversion.asByteBuffer(memory, bufferSize).order(ByteOrder.BIG_ENDIAN);
  }

  @SuppressWarnings("unchecked")
  private static long handleJvmMessage(long threadId, long id, long address, long size)
      throws Throwable {
    var channel = ID_TO_CHANNEL.get(id);
    channel.otherIsolateThread.set(threadId);
    var seg = MemorySegment.ofAddress(address).reinterpret(size);
    var reply = handleWithChannel(channel, seg.asByteBuffer());
    return reply;
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

  /**
   * Set of methods necessary for construction of a {@link Channel}. Subclasses must have a public
   * default constructor accessible via reflection from the {@link Channel#create} method.
   */
  public abstract static class Config {
    /**
     * Creates instance of pool for persisting messages.
     *
     * @param channel the channel associated with this {@code Config}
     * @return the pool to use when sending messages
     */
    public abstract Persistance.Pool createPool(Channel<?> channel);
  }
}
