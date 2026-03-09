package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import com.oracle.truffle.api.nodes.Node;
import java.io.IOException;
import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.graalvm.polyglot.Value;

/** Sends a message to the other side with ReflectionLibrary-like arguments. */
@Persistable(id = 81901)
public record OtherJvmMessage(long id, Message message, List<Object> args)
    implements Function<
        Channel<OtherJvmPool>, OtherJvmResult<? extends Object, ? extends Exception>> {
  private static final Message IS_IDENTICAL = Message.resolve(InteropLibrary.class, "isIdentical");
  private static final Message IS_POINTER = Message.resolve(InteropLibrary.class, "isPointer");
  private static final Message AS_POINTER = Message.resolve(InteropLibrary.class, "asPointer");

  @Override
  public OtherJvmResult<? extends Object, ? extends Exception> apply(Channel<OtherJvmPool> t) {
    var lib = ReflectionLibrary.getUncached();
    var prev = t.getConfig().enter(t, lib);
    try {
      var receiver = t.getConfig().findObject(id());
      if (receiver == null) {
        throw new NullPointerException(
            "No object for " + id() + " message: " + message() + " args: " + args());
      }
      var iop = InteropLibrary.getUncached();
      if (message == IS_IDENTICAL) {
        args.set(1, iop);
      }
      if (message == IS_POINTER && iop.hasBufferElements(receiver)) {
        var buf = Value.asValue(receiver).as(ByteBuffer.class);
        return new ReturnValue<>(buf.isDirect());
      }
      if (message == AS_POINTER && iop.hasBufferElements(receiver)) {
        var buf = Value.asValue(receiver).as(ByteBuffer.class);
        var seg = MemorySegment.ofBuffer(buf);
        return new ReturnValue<>(seg.address());
      }
      var res = lib.send(receiver, message, args.toArray());
      return new ReturnValue<>(res);
    } catch (Exception ex) {
      return ThrowException.create(ex);
    } finally {
      t.getConfig().leave(t, lib, prev);
    }
  }

  @Persistable(id = 81908, allowInlining = false)
  record ReturnValue<T, E extends Exception>(T value) implements OtherJvmResult<T, E> {
    static <T, E extends Exception> ReturnValue<T, E> create(T value) {
      return new ReturnValue<>(value);
    }

    @Override
    public T value(Node location) throws E {
      return value();
    }
  }

  @Persistable(id = 81909, allowInlining = false)
  record ThrowValue<T, E extends Throwable>(Optional<String> msg, TruffleObject exception)
      implements OtherJvmResult<T, E> {
    @Override
    @SuppressWarnings("unchecked")
    public T value(Node location) throws E {
      var ex = exception();
      var msg = msg().isPresent() ? msg().get() : null;
      assert InteropLibrary.getUncached().isException(ex);
      if (ex instanceof AbstractTruffleException truffleEx) {
        throw truffleEx;
      } else {
        throw new OtherJvmTruffleException(msg, (OtherJvmObject) ex, location);
      }
    }
  }

  @Persistable(id = 81910, allowInlining = false)
  record ThrowException<V, E extends Throwable>(
      int kind, Optional<String> msg, List<StackTraceElement> stack)
      implements OtherJvmResult<V, E> {
    private static final Map<Class<? extends Throwable>, Integer> kinds;

    static {
      kinds = new LinkedHashMap<>();
      kinds.put(ClassNotFoundException.class, 1);
      kinds.put(UnsupportedMessageException.class, 2);
      kinds.put(UnknownIdentifierException.class, 3);
      kinds.put(UnsupportedTypeException.class, 4);
      kinds.put(InvalidArrayIndexException.class, 5);
      kinds.put(IllegalArgumentException.class, 6);
      kinds.put(IllegalStateException.class, 7);
    }

    @SuppressWarnings("unchecked")
    static <T, E extends Throwable> OtherJvmResult<T, E> create(E ex) {
      var msg = Optional.ofNullable(ex.getMessage());
      if (ex instanceof OtherJvmTruffleException truffleEx) {
        var original = truffleEx.delegate;
        return new ThrowValue<>(msg, original);
      } else if (InteropLibrary.getUncached().isException(ex)
          && ex instanceof TruffleObject truffleEx) {
        return new ThrowValue<>(msg, truffleEx);
      } else {
        var kind = kinds.getOrDefault(ex.getClass(), 0);
        var stack = List.of(ex.getStackTrace());
        if (kind == 0) {
          var classWithMsg = ex.getClass().getName() + ": " + ex.getMessage();
          return new ThrowException<>(kind, Optional.of(classWithMsg), stack);
        } else {
          return new ThrowException<>(kind, msg, stack);
        }
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    public V value(Node who) throws E {
      var msgOrNull = msg().isPresent() ? msg().get() : null;
      var ex =
          switch (kind) {
            case 1 -> new ClassNotFoundException(msgOrNull);
            case 2 -> UnsupportedMessageException.create();
            case 3 -> UnknownIdentifierException.create(msgOrNull);
            case 4 -> UnsupportedTypeException.create(new Object[0], msgOrNull);
            case 5 -> {
              int index;
              try {
                var words = msgOrNull.split("[ \\.]");
                index = Integer.parseInt(words[3]);
              } catch (NullPointerException
                  | NumberFormatException
                  | IndexOutOfBoundsException recover) {
                index = -1;
              }
              yield InvalidArrayIndexException.create(index);
            }
            case 6 -> new IllegalArgumentException(msgOrNull);
            case 7 -> new IllegalStateException(msgOrNull);
            default -> new OtherJvmException(msgOrNull);
          };
      ex.setStackTrace(stack().toArray(StackTraceElement[]::new));
      throw (E) ex;
    }
  }

  @Persistable(id = 81905)
  public record LoadClass(String name)
      implements Function<
          Channel<OtherJvmPool>, OtherJvmResult<TruffleObject, ClassNotFoundException>> {
    @Override
    public OtherJvmResult<TruffleObject, ClassNotFoundException> apply(Channel<OtherJvmPool> t) {
      assert !t.isMaster() : "Class loading only works on the slave side!";
      try {
        var clazzRaw = t.getConfig().loadClassObject(t, name);
        return ReturnValue.create(clazzRaw);
      } catch (ClassNotFoundException ex) {
        return ThrowException.create(ex);
      }
    }
  }

  @Persistable(id = 81906)
  public record AddToClassPath(String path) implements Function<Channel<OtherJvmPool>, Void> {
    @Override
    public Void apply(Channel<OtherJvmPool> t) {
      t.getConfig().addToClassPath(t, path);
      return null;
    }
  }

  @Persistable(id = 81907)
  public record FindLibraries(TruffleObject callback)
      implements Function<Channel<OtherJvmPool>, Void> {
    @Override
    public Void apply(Channel<OtherJvmPool> t) {
      t.getConfig().findLibraries(t, callback);
      return null;
    }
  }

  /**
   * Sent from the other JVM to report that it no longer keeps reference to object with ID {@code
   * id}.
   */
  @Persistable(id = 81911)
  public static record GC(long id) implements Function<Channel<OtherJvmPool>, Void> {
    @Override
    public Void apply(Channel<OtherJvmPool> t) {
      t.getConfig().gc(id);
      return null;
    }
  }

  /** Requests polyglot bindings from the other side. */
  @Persistable(id = 81912)
  public static record PolyglotBindings(String name)
      implements Function<Channel<OtherJvmPool>, Object> {
    @Override
    public Object apply(Channel<OtherJvmPool> t) {
      return t.getConfig().getBindings(name);
    }
  }

  @Persistable(id = 81914)
  static final class PersistStackTraceElement extends Persistance<java.lang.StackTraceElement> {
    public PersistStackTraceElement() {
      super(java.lang.StackTraceElement.class, false, 81914);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected java.lang.StackTraceElement readObject(Persistance.Input in) throws IOException {
      var declaringClass = in.readUTF();
      var methodName = in.readUTF();
      var fileName = in.readUTF();
      var lineNumber = in.readInt();
      return new java.lang.StackTraceElement(declaringClass, methodName, fileName, lineNumber);
    }

    @Override
    protected void writeObject(java.lang.StackTraceElement obj, Persistance.Output out)
        throws IOException {
      out.writeUTF(obj.getClassName());
      out.writeUTF(obj.getMethodName());
      out.writeUTF(obj.getFileName());
      out.writeInt(obj.getLineNumber());
    }
  }
}
