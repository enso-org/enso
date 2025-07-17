package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import java.util.Arrays;
import org.enso.jvm.channel.Channel;

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

  private final Channel<OtherJvmPool> channel;
  private final long id;

  OtherJvmObject(Channel<OtherJvmPool> channel, long id) {
    this.channel = channel;
    this.id = id;
  }

  long id() {
    return id;
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
      // proper dispatch to the other JVM
      var msg = new OtherJvmMessage(id, message, Arrays.asList(args));
      var reply = channel.execute(OtherJvmResult.class, msg);
      return reply.value();
    }
  }

  static Object bindToChannel(Object v, Channel<OtherJvmPool> ch) {
    if (v instanceof OtherJvmObject toBind) {
      assert toBind.channel == null;
      return new OtherJvmObject(ch, toBind.id);
    } else {
      return v;
    }
  }
}
