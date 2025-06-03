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

  /** The SubstrateVM side of a channel. */
  private Channel(long id, Persistance.Pool pool, JNI.JNIEnv env) {
    this.id = id;
    this.pool = pool;
    this.env = env;
    this.isolate = -1;
    this.callbackFn = -1;
  }

  /** The HotSpot JVM side of a channel. */
  private Channel(long id, Persistance.Pool pool, long isolate, long callbackFn) {
    if (ImageInfo.inImageCode()) {
      throw new IllegalStateException("Only usable in HotSpot");
    }
    this.id = id;
    this.pool = pool;
    this.env = null;
    this.isolate = isolate;
    this.callbackFn = callbackFn;
  }

  /**
   * Factory method to initialize the Channel in the SubstrateVM.
   *
   * @param e
   * @return
   */
  static synchronized Channel create(JNI.JNIEnv e) {
    var id = idCounter++;

    var classNameWithSlashes = "org/enso/os/environment/jni/Channel";
    var methodName = "createJvmPeerChannel";
    try (var classInC = CTypeConversion.toCString(classNameWithSlashes);
        var methodInC = CTypeConversion.toCString(methodName);
        var signatureInC = CTypeConversion.toCString("(JJJ)Z")) {
      var fn = e.getFunctions();
      var clazz = fn.getFindClass().call(e, classInC.get());
      assert clazz.isNonNull() : "Class not found " + classNameWithSlashes;
      var method = fn.getGetStaticMethodID().call(e, clazz, methodInC.get(), signatureInC.get());
      assert method.isNonNull() : "method not found in " + classNameWithSlashes;
      var arg = StackValue.get(2, JNI.JValue.class);
      arg.addressOf(0).setLong(id);
      arg.addressOf(1).setLong(CurrentIsolate.getCurrentThread().rawValue());
      arg.addressOf(2).setLong(CALLBACK_FN.getFunctionPointer().rawValue());
      var replyOk = fn.getCallStaticBooleanMethodA().call(e, clazz, method, arg);
      assert replyOk : "Failed to create peer in HotSpot JVM";

      var channel = new Channel(id, JVMPeer.POOL, e);
      ID_TO_CHANNEL.put(id, channel);
      return channel;
    }
  }

  /** Allocates new channel with given ID in the HotSpot VM. Called via JNI/foreign interface. */
  private static boolean createJvmPeerChannel(long id, long threadId, long callbackFn) {
    var channel = new Channel(id, JVMPeer.POOL, threadId, callbackFn);
    var prev = ID_TO_CHANNEL.put(id, channel);
    return prev == null;
  }

  /**
   * <em>Executes a message</em> in the other JVM. The message is any subclass of {@link Message}
   * registered for persistance via {@link Persistable @Persistable} annotation into the {@link
   * Persistance.Pool pool associated with this JVM}. The result (which is of type {@code R}) also
   * has to be registered for serde.
   *
   * @param msg the message that gets serialized, transferred into the other JVM, deserialized on
   *     the other side and {@link Message#evaluate() evaluated} there
   * @param <R> the type of result we expect the message to return
   * @return the value gets computed via {@link Message#evaluate()} in the other JVM and then it
   *     gets serialized and transferred back to us. Deserialized and the value is then returned
   *     from this method
   */
  public final <R> R execute(Message<R> msg) {
    if (this.isolate == -1) {
      return executeImpl(pool, msg, memory -> toHotSpotMessage(env, id, memory));
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
    var ref = JVMPeer.POOL.read(buf, null);
    var msg = ref.get(Channel.Message.class);
    var res = msg.evaluate(channel);
    var bytes = Persistables.POOL.write(res, null);
    seg.copyFrom(MemorySegment.ofArray(bytes));
    return bytes.length;
  }

  private static long toHotSpotMessage(JNI.JNIEnv e, long id, MemorySegment segment) {
    var classNameWithSlashes = "org/enso/os/environment/jni/Channel";
    var methodName = "handleJvmMessage";
    try (var classInC = CTypeConversion.toCString(classNameWithSlashes);
        var methodInC = CTypeConversion.toCString(methodName);
        var signatureInC = CTypeConversion.toCString("(JJJ)J")) {
      var fn = e.getFunctions();
      var clazz = fn.getFindClass().call(e, classInC.get());
      assert clazz.isNonNull() : "Class not found " + classNameWithSlashes;
      var method = fn.getGetStaticMethodID().call(e, clazz, methodInC.get(), signatureInC.get());
      assert method.isNonNull() : "method not found in " + classNameWithSlashes;
      long address = segment.address();
      assert address > 0 : "We need an address";
      var arg = StackValue.get(3, JNI.JValue.class);
      arg.addressOf(0).setLong(id);
      arg.addressOf(1).setLong(address);
      arg.addressOf(2).setLong(segment.byteSize());
      var replySize = fn.getCallStaticLongMethodA().call(e, clazz, method, arg);
      return replySize;
    }
  }

  static <R> R executeImpl(
      Persistance.Pool pool, Channel.Message<R> msg, Function<MemorySegment, Long> send) {
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

  private static long handleJvmMessage(long id, long address, long size) throws Throwable {
    var channel = ID_TO_CHANNEL.get(id);
    return handleWithChannel(channel, address, size);
  }

  @Override
  public void close() throws Exception {
    ID_TO_CHANNEL.remove(id, this);
    // TBD remove on the peer as well
  }

  /**
   * Subclasses of message denote a computational task to be performed in the "other {@link JVM}".
   *
   * @param <R> type of the return value
   */
  public abstract static class Message<R> {

    final Class<R> replyType;

    /**
     * Constructor for subclasses. Use it as {@code super(Integer.class)} to specify the reply type
     * of the exception which is then returned from the {@link #evaluate} method.
     *
     * @param replyType the type of the reply
     */
    protected Message(Class<R> replyType) {
      this.replyType = replyType;
    }

    /**
     * Handles evaluation of the exception. Use {@link Channel#execute} to pass this messages to the
     * other {@link JVM}. After all the serde and transfer to the other {@link JVM} this method is
     * executed to perform its operation. Then the result is passed back via serde again and
     * returned from the {@link Channel#execute} method.
     *
     * @param channel allows sending messages to the other JVM
     * @return the result of the evaluation or {@code null}
     * @throws Throwable the computation may yield exceptions or errors which are then transferred
     *     back to the callee JVM
     */
    protected abstract R evaluate(Channel channel) throws Throwable;
  }
}
