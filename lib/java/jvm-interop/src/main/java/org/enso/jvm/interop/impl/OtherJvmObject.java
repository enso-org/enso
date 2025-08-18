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
import java.util.List;
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

  private static final Message IS_STRING = Message.resolve(InteropLibrary.class, "isString");
  private static final Message IS_META_OBJECT =
      Message.resolve(InteropLibrary.class, "isMetaObject");
  private static final Message HAS_META_PARENTS =
      Message.resolve(InteropLibrary.class, "hasMetaParents");
  private static final Message GET_META_PARENTS =
      Message.resolve(InteropLibrary.class, "getMetaParents");
  private static final Message GET_META_QUALIFIED_NAME =
      Message.resolve(InteropLibrary.class, "getMetaQualifiedName");
  private static final Message IS_NULL = Message.resolve(InteropLibrary.class, "isNull");
  private static final Message HAS_ARRAY_ELEMENTS =
      Message.resolve(InteropLibrary.class, "hasArrayElements");

  private final Channel<OtherJvmPool> channel;
  private final long id;
  private Boolean isMetaObject;
  private Boolean isNull;
  private Boolean hasArrayElements;
  private String metaQualifiedName;

  private Object cachedMetaParents;

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
        || IS_STRING == message
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
      if (message == HAS_META_PARENTS || message == GET_META_PARENTS) {
        if (!isMetaObject) {
          throw UnsupportedMessageException.create();
        }
        if (cachedMetaParents == null) {
          var msg = new OtherJvmMessage(id, GET_META_PARENTS, List.of());
          var reply = executeMessage(msg, message, args);
          try {
            var arr = reply.value();
            var iop = InteropLibrary.getUncached();
            var len = Math.toIntExact(iop.getArraySize(arr));
            var copy = new OtherJvmObject[len];
            for (var i = 0; i < len; i++) {
              copy[i] = (OtherJvmObject) iop.readArrayElement(arr, i);
            }
            cachedMetaParents = copy;
          } catch (UnsupportedMessageException ex) {
            cachedMetaParents = ex;
          }
        }
        return switch (cachedMetaParents) {
          case UnsupportedMessageException ex -> {
            if (message == GET_META_PARENTS) {
              yield new OtherArray();
            } else {
              yield false;
            }
          }
          case OtherJvmObject[] arr -> {
            if (message == GET_META_PARENTS) {
              yield new OtherArray(arr);
            } else {
              yield true;
            }
          }
          default -> throw new IllegalStateException();
        };
      }
      if (message == GET_META_QUALIFIED_NAME && metaQualifiedName != null) {
        return metaQualifiedName;
      }
      if (message == IS_NULL && isNull != null) {
        return isNull;
      }
      if (message == HAS_ARRAY_ELEMENTS && hasArrayElements != null) {
        return hasArrayElements;
      }

      // proper dispatch to the other JVM
      var msg = new OtherJvmMessage(id, message, Arrays.asList(args));
      var reply = executeMessage(msg, message, args);
      var result = reply.value();
      return result;
    }
  }

  private OtherJvmResult<?, ?> executeMessage(OtherJvmMessage msg, Message message, Object[] args) {
    var reply = channel.execute(OtherJvmResult.class, msg);
    channel.getConfig().profileMessage(message, args);
    return reply;
  }

  @SuppressWarnings("unchecked")
  private static OtherJvmObject bindToChannel(
      OtherJvmObject toBind,
      Channel<OtherJvmPool> ch,
      Function<OtherJvmObject, OtherJvmObject> findCached) {
    assert toBind.channel == null;
    var other = new OtherJvmObject(ch, toBind.id);
    other.isMetaObject = toBind.isMetaObject;
    other.metaQualifiedName = toBind.metaQualifiedName;
    other.isNull = toBind.isNull;
    other.hasArrayElements = toBind.hasArrayElements;
    if (Boolean.TRUE.equals(toBind.isMetaObject)) {
      return findCached.apply(other);
    } else {
      return other;
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
    out.writeBoolean(hasArrayElements != null);
    if (hasArrayElements != null) {
      out.writeBoolean(hasArrayElements);
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
    if (in.readBoolean()) {
      other.hasArrayElements = in.readBoolean();
    }
    return other;
  }

  static Object readResolve(
      Channel<OtherJvmPool> channel,
      Object obj,
      Function<Long, TruffleObject> findObject,
      Function<OtherJvmObject, OtherJvmObject> findCached) {
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
          var proxy = OtherJvmObject.bindToChannel(other, channel, findCached);
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
        if (iop.isString(foreign)) {
          try {
            yield iop.asString(foreign);
          } catch (UnsupportedMessageException ex) {
            // let it be and return normal delegate
          }
        }
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
        other.hasArrayElements = iop.hasArrayElements(foreign);
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
