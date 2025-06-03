package org.enso.os.environment.jni;

import java.lang.foreign.FunctionDescriptor;
import java.lang.foreign.Linker;
import java.lang.foreign.MemorySegment;
import java.lang.foreign.ValueLayout;
import org.enso.persist.Persistance;
import org.graalvm.nativeimage.CurrentIsolate;
import org.graalvm.nativeimage.ImageInfo;
import org.graalvm.nativeimage.StackValue;
import org.graalvm.nativeimage.c.type.CTypeConversion;

/** Channel connects two {@link JVM} instances. */
public final class Channel {

  /** persistance pool associated with this channel object */
  private final Persistance.Pool pool;

  private final JNI.JNIEnv env;
  private final long isolate;
  private final long callbackFn;

  /* private */
  Channel(Persistance.Pool pool, JNI.JNIEnv env) {
    this.pool = pool;
    this.env = env;
    this.isolate = -1;
    this.callbackFn = -1;
  }

  /* private */
  Channel(Persistance.Pool pool, long isolate, long callbackFn) {
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
   * @param msg the message that gets serialized, transfered into the other JVM, deserialized on the
   *     other side and {@link Message#evaluate() evaluated} there
   * @param <R> the type of result we expect the message to return
   * @return the value gets computed via {@link Message#evaluate()} in the other JVM and then it
   *     gets serialized and transfered back to us. Deserialized and the value is then returned from
   *     this method
   */
  public final <R> R execute(Message<R> msg) {
    if (this.isolate == -1) {
      return JVM.executeImpl(pool, msg, memory -> toHotSpotMessage(env, memory));
    } else {
      java.lang.foreign.MemorySegment fnCallbackAddress = MemorySegment.ofAddress(callbackFn);
      java.lang.foreign.FunctionDescriptor fnDescriptor =
          FunctionDescriptor.of(
              ValueLayout.JAVA_LONG,
              ValueLayout.ADDRESS,
              ValueLayout.ADDRESS,
              ValueLayout.JAVA_LONG);
      java.lang.invoke.MethodHandle fnHandle =
          Linker.nativeLinker().downcallHandle(fnCallbackAddress, fnDescriptor);
      return JVM.executeImpl(
          pool,
          msg,
          seg -> {
            Object res = -1L;
            try {
              java.lang.foreign.MemorySegment isoRef = MemorySegment.ofAddress(isolate);
              res = fnHandle.invoke(isoRef, seg, seg.byteSize());
            } catch (Throwable ex) {
              ex.printStackTrace();
            }
            return (long) res;
          });
    }
  }

  private static long toHotSpotMessage(JNI.JNIEnv e, MemorySegment segment) {
    java.lang.String classNameWithSlashes = "org/enso/os/environment/jni/JVMPeer";
    java.lang.String methodName = "handle";
    try (org.graalvm.nativeimage.c.type.CTypeConversion.CCharPointerHolder classInC =
            CTypeConversion.toCString(classNameWithSlashes);
        org.graalvm.nativeimage.c.type.CTypeConversion.CCharPointerHolder methodInC =
            CTypeConversion.toCString(methodName);
        org.graalvm.nativeimage.c.type.CTypeConversion.CCharPointerHolder signatureInC =
            CTypeConversion.toCString("(JJJJ)J")) {
      org.enso.os.environment.jni.JNINativeInterface fn = e.getFunctions();
      org.enso.os.environment.jni.JNI.JClass clazz = fn.getFindClass().call(e, classInC.get());
      assert clazz.isNonNull() : "Class not found " + classNameWithSlashes;
      org.enso.os.environment.jni.JNI.JMethodID method =
          fn.getGetStaticMethodID().call(e, clazz, methodInC.get(), signatureInC.get());
      assert method.isNonNull() : "method not found in " + classNameWithSlashes;
      long address = segment.address();
      assert address > 0 : "We need an address";
      org.enso.os.environment.jni.JNI.JValue arg = StackValue.get(4, JNI.JValue.class);
      arg.addressOf(0).setLong(CurrentIsolate.getCurrentThread().rawValue());
      arg.addressOf(1).setLong(JVM.CALLBACK_FN.getFunctionPointer().rawValue());
      arg.addressOf(2).setLong(address);
      arg.addressOf(3).setLong(segment.byteSize());
      long replySize = fn.getCallStaticLongMethodA().call(e, clazz, method, arg);
      return replySize;
    }
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
