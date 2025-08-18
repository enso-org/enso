package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import java.io.IOException;
import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistance;

@ExportLibrary(ReflectionLibrary.class)
final class OtherJvmObject implements TruffleObject {
  /** receiver for other than InteropLibrary messages */
  private static final Object POJO = new Object();

  /** special message */
  private static final Message HAS_LANGUAGE = Message.resolve(InteropLibrary.class, "hasLanguage");

  private static final Message GET_LANGUAGE = Message.resolve(InteropLibrary.class, "getLanguage");

  private static final Message IS_IDENTICAL_OR_UNDEFINED =
      Message.resolve(InteropLibrary.class, "isIdenticalOrUndefined");
  private static final Message IS_IDENTICAL = Message.resolve(InteropLibrary.class, "isIdentical");
  private static final Message HAS_SOURCE_LOCATION =
      Message.resolve(InteropLibrary.class, "hasSourceLocation");
  private static final Message GET_SOURCE_LOCATION =
      Message.resolve(InteropLibrary.class, "getSourceLocation");

  private static final Message IS_META_OBJECT =
      Message.resolve(InteropLibrary.class, "isMetaObject");
  private static final Message GET_META_QUALIFIED_NAME =
      Message.resolve(InteropLibrary.class, "getMetaQualifiedName");
  private static final Message IS_NULL = Message.resolve(InteropLibrary.class, "isNull");

  private final Channel<OtherJvmPool> channel;
  private final long id;
  private Boolean isMetaObject;
  private Boolean isNull;
  private String metaQualifiedName;

  private OtherJvmObject(Channel<OtherJvmPool> channel, long id) {
    this.channel = channel;
    this.id = id;
  }

  long id() {
    return id;
  }

  @Override
  public String toString() {
    return "OtherJvmObject{" + "id=" + id + '}';
  }

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  Object send(Message message, Object[] args) throws Exception {
    if (message == IS_IDENTICAL) {
      if (args[0] instanceof OtherJvmObject other) {
        if (id() == other.id()) {
          return true;
        } else {
          // fall thru but without the library
          args[1] = null;
        }
      } else {
        return false;
      }
    }
    if (message.getLibraryClass() != InteropLibrary.class
        || HAS_LANGUAGE == message
        || GET_LANGUAGE == message
        || HAS_SOURCE_LOCATION == message
        || GET_SOURCE_LOCATION == message
        || IS_IDENTICAL_OR_UNDEFINED == message) {
      // we need to invoke default implementation of library
      // to handle the message in a proper way
      // hence provide POJO as a receiver
      return ReflectionLibrary.getUncached().send(POJO, message, args);
    } else {
      if (message == IS_META_OBJECT && isMetaObject != null) {
        return isMetaObject;
      }
      if (message == GET_META_QUALIFIED_NAME && metaQualifiedName != null) {
        return metaQualifiedName;
      }
      if (message == IS_NULL && isNull != null) {
        return isNull;
      }

      // proper dispatch to the other JVM
      var msg = new OtherJvmMessage(id, message, Arrays.asList(args));
      var reply = channel.execute(OtherJvmResult.class, msg);
      channel.getConfig().profileMessage(message, args);
      var result = reply.value();
      return result;
    }
  }

  @SuppressWarnings("unchecked")
  private static <T> T bindToChannel(T v, Channel<OtherJvmPool> ch) {
    if (v instanceof OtherJvmObject toBind) {
      assert toBind.channel == null;
      var other = new OtherJvmObject(ch, toBind.id);
      other.isMetaObject = toBind.isMetaObject;
      other.metaQualifiedName = toBind.metaQualifiedName;
      other.isNull = toBind.isNull;
      return (T) other;
    } else {
      return v;
    }
  }

  final void writeTo(Persistance.Output out) throws IOException {
    out.writeLong(id());
    out.writeBoolean(isMetaObject != null);
    if (isMetaObject != null) {
      out.writeBoolean(isMetaObject);
    }
    out.writeBoolean(metaQualifiedName != null);
    if (metaQualifiedName != null) {
      out.writeUTF(metaQualifiedName);
    }
    out.writeBoolean(isNull != null);
    if (isNull != null) {
      out.writeBoolean(isNull);
    }
  }

  static OtherJvmObject readFrom(Persistance.Input in) throws IOException {
    var other = new OtherJvmObject(null, in.readLong());
    if (in.readBoolean()) {
      other.isMetaObject = in.readBoolean();
    }
    if (in.readBoolean()) {
      other.metaQualifiedName = in.readUTF();
    }
    if (in.readBoolean()) {
      other.isNull = in.readBoolean();
    }
    return other;
  }

  static Object readResolve(
      Channel<OtherJvmPool> channel, Object obj, Function<Long, TruffleObject> findObject) {
    return switch (obj) {
      case OtherJvmObject other -> {
        if (other.id() < 0) {
          // the other object with negative number came back
          // it is our own object
          var ourOwn = findObject.apply(-other.id());
          assert ourOwn != null;
          yield ourOwn;
        } else {
          // real truffle object in the other JVM
          // need to keep it as OtherJvmObject proxy
          // just associate channel to it
          var proxy = OtherJvmObject.bindToChannel(other, channel);
          yield proxy;
        }
      }
      case null -> null;
      default -> obj;
    };
  }

  static Object writeReplace(Object obj, BiFunction<TruffleObject, Boolean, Long> registerObject) {
    return switch (obj) {
      case OtherJvmObject other -> {
        // returning back their own OtherJvmObject - let
        // them know it is theirs by using negative ID
        yield new OtherJvmObject(null, -other.id());
      }
      case OtherJvmTruffleException ex -> {
        // unwrap the exception to object reference
        // and send it back as regular OtherJvmObject
        yield new OtherJvmObject(null, -ex.delegate.id());
      }
      case TruffleObject foreign -> {
        var iop = InteropLibrary.getUncached();
        var meta = iop.isMetaObject(foreign);
        var id = registerObject.apply(foreign, meta);
        // our own truffle objects send to the other side should
        // have a positive ID
        var other = new OtherJvmObject(null, id);
        other.isMetaObject = meta;
        if (other.isMetaObject) {
          try {
            other.metaQualifiedName = iop.asString(iop.getMetaQualifiedName(foreign));
          } catch (UnsupportedMessageException ex) {
            // go without qualified name
          }
        }
        other.isNull = iop.isNull(foreign);
        yield other;
      }
      case null -> null;
      default -> obj;
    };
  }

  final boolean assertChannel(Channel ch) {
    return ch == channel;
  }
}
