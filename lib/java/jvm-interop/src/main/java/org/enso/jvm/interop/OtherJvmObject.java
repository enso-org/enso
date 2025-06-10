package org.enso.jvm.interop;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import java.util.List;
import org.enso.jvm.channel.Channel;

@ExportLibrary(ReflectionLibrary.class)
final class OtherJvmObject implements TruffleObject {
  private final Channel channel;
  private final long id;

  OtherJvmObject(Channel channel, long id) {
    assert id > 0;
    this.channel = channel;
    this.id = id;
  }

  long id() {
    return id;
  }

  @CompilerDirectives.TruffleBoundary
  @ExportMessage
  Object send(Message message, Object[] args) throws Exception {
    if (message.getLibraryClass() != InteropLibrary.class) {
      if (message.getParameterCount() == 1 && message.getReturnType() == boolean.class) {
        return false;
      }
      throw UnsupportedMessageException.create();
    }
    var msg = new OtherJvmMessage(id, message, List.of(args));
    var reply = channel.execute(OtherJvmResult.class, msg);
    return bindToChannel(reply.value(), channel);
  }

  static Object bindToChannel(Object v, Channel ch) {
    if (v instanceof OtherJvmObject toBind) {
      assert toBind.channel == null;
      return new OtherJvmObject(ch, toBind.id);
    } else {
      return v;
    }
  }
}
