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
import java.lang.foreign.MemorySegment;
import java.nio.ByteBuffer;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistable;
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
  record ThrowValue<T, E extends Exception>(Optional<String> msg, TruffleObject exception)
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
  record ThrowException<V, E extends Exception>(int kind, Optional<String> msg)
      implements OtherJvmResult<V, E> {
    private static final Map<Class<? extends Throwable>, Integer> kinds;

    static {
      kinds = new LinkedHashMap<>();
      kinds.put(ClassNotFoundException.class, 1);
      kinds.put(UnsupportedMessageException.class, 2);
      kinds.put(UnknownIdentifierException.class, 3);
      kinds.put(UnsupportedTypeException.class, 4);
      kinds.put(InvalidArrayIndexException.class, 5);
    }

    @SuppressWarnings("unchecked")
    static <T, E extends Exception> OtherJvmResult<T, E> create(E ex) {
      var msg = Optional.ofNullable(ex.getMessage());
      if (ex instanceof OtherJvmTruffleException truffleEx) {
        var original = truffleEx.delegate;
        return new ThrowValue<>(msg, original);
      } else if (InteropLibrary.getUncached().isException(ex)
          && ex instanceof TruffleObject truffleEx) {
        return new ThrowValue<>(msg, truffleEx);
      } else {
        var kind = kinds.getOrDefault(ex.getClass(), 0);
        return new ThrowException<>(kind, msg);
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    public V value(Node who) throws E {
      var msg = msg().isPresent() ? msg().get() : null;
      switch (kind) {
        case 1 -> throw (E) new ClassNotFoundException(msg);
        case 2 -> throw (E) UnsupportedMessageException.create();
        case 3 -> throw (E) UnknownIdentifierException.create(msg);
        case 4 -> throw (E) UnsupportedTypeException.create(new Object[0], msg);
        case 5 -> {
          int index;
          try {
            var words = msg.split("[ \\.]");
            index = Integer.parseInt(words[3]);
          } catch (NullPointerException | NumberFormatException | IndexOutOfBoundsException ex) {
            index = -1;
          }
          throw (E) InvalidArrayIndexException.create(index);
        }
        default -> throw new OtherJvmException(msg);
      }
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
}
