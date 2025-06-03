package org.enso.os.environment.jni;

import java.io.IOException;
import java.lang.foreign.Arena;
import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;
import org.enso.persist.Persistance;
import org.graalvm.nativeimage.CurrentIsolate;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.IsolateThread;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.function.CEntryPointLiteral;
import org.graalvm.nativeimage.c.function.CFunctionPointer;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;

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
  private final long callbackFn;
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
    this.callbackFn = -1;
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
    this.callbackFn = callbackFn;
    this.env = null;
    this.channelClass = null;
    this.channelHandle = null;
  }

  /**
   * Factory method to initialize the Channel in the SubstrateVM.
   *
   * @param e JNI environment to talk to the HotSpot JVM
   * @param poolClass the class which has public default constructor and can suply instance of
   *     persistance pool to use for communication
   * @return channel for sending messages to the HotSpot JVM
   */
  static synchronized Channel create(
      JNI.JNIEnv e, Class<? extends Supplier<Persistance.Pool>> poolClass) {
    var id = idCounter++;
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
      var arg = StackValue.get(4, JNI.JValue.class);
      arg.addressOf(0).setLong(id);
      arg.addressOf(1).setLong(CurrentIsolate.getCurrentThread().rawValue());
      arg.addressOf(2).setLong(CALLBACK_FN.getFunctionPointer().rawValue());
      arg.addressOf(3).setJObject(poolClassInHotSpot);
      var replyOk = fn.getCallStaticBooleanMethodA().call(e, channelClass, createMethod, arg);
      if (!replyOk) {
        fn.getExceptionDescribe().call(e);
      }
      assert replyOk : "Failed to create peer in HotSpot JVM";

      var handleMethod =
          fn.getGetStaticMethodID().call(e, channelClass, handleInC.get(), handleSigInC.get());

      try {
        var pool = poolClass.getConstructor().newInstance().get();
        var channel = new Channel(id, pool, e, channelClass, handleMethod);
        ID_TO_CHANNEL.put(id, channel);
        return channel;
      } catch (ReflectiveOperationException ex) {
        throw new IllegalStateException(ex);
      }
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
    if (this.isolate == -1) {
      return executeImpl(pool, resultType, msg, memory -> toHotSpotMessage(memory));
    } else {
      var fnCallbackAddress = MemorySegment.ofAddress(callbackFn);
      var fnDescriptor =
          FunctionDescriptor.of(
              ValueLayout.JAVA_LONG,
              ValueLayout.ADDRESS,
              ValueLayout.JAVA_LONG,
              ValueLayout.ADDRESS,
              ValueLayout.JAVA_LONG);
      var fnHandle = Linker.nativeLinker().downcallHandle(fnCallbackAddress, fnDescriptor);
      return executeImpl(
          pool,
          resultType,
          msg,
          seg -> {
            Object res = -1L;
            try {
              var isoRef = MemorySegment.ofAddress(isolate);
              res = fnHandle.invoke(isoRef, id, seg, seg.byteSize());
            } catch (Throwable ex) {
              ex.printStackTrace();
            }
            return (long) res;
          });
    }
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
    var len = handleWithChannel(channel, data.rawValue(), size);
    return len;
  }

  private static long handleWithChannel(Channel channel, long address, long size) throws Throwable {
    var seg = MemorySegment.ofAddress(address).reinterpret(size);
    var buf = seg.asByteBuffer();
    var ref = channel.pool.read(buf, null);
    var msg = ref.get(Function.class);
    @SuppressWarnings("unchecked")
    var res = msg.apply(channel);
    var bytes = Persistables.POOL.write(res, null);
    seg.copyFrom(MemorySegment.ofArray(bytes));
    return bytes.length;
  }

  private long toHotSpotMessage(MemorySegment segment) {
    var fn = env.getFunctions();
    long address = segment.address();
    assert address > 0 : "We need an address";
    var arg = StackValue.get(3, JNI.JValue.class);
    arg.addressOf(0).setLong(id);
    arg.addressOf(1).setLong(address);
    arg.addressOf(2).setLong(segment.byteSize());
    var replySize = fn.getCallStaticLongMethodA().call(env, channelClass, channelHandle, arg);
    return replySize;
  }

  static <R> R executeImpl(
      Persistance.Pool pool,
      Class<R> replyType,
      Function<Channel, R> msg,
      Function<MemorySegment, Long> send) {
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
      return result.get(replyType);
    } catch (IOException ex) {
      throw new IllegalStateException(ex);
    }
  }

  private static long handleJvmMessage(long id, long address, long size) throws Throwable {
    var channel = ID_TO_CHANNEL.get(id);
    return handleWithChannel(channel, address, size);
  }

  @Override
  public void close() throws Exception {
    ID_TO_CHANNEL.remove(id, this);
    // TBD remove on the peer as well
  }
}
