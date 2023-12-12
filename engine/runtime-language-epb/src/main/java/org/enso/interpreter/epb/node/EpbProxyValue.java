package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;

@ExportLibrary(value = ReflectionLibrary.class)
final class EpbProxyValue implements TruffleObject {
  private static final Object DEFAULT = new Object();

  private final GuardedTruffleContext ctx;
  private final Object value;

  private EpbProxyValue(Object v, GuardedTruffleContext ctx) {
    this.value = v;
    this.ctx = ctx;
  }

  static Object wrap(Object obj, GuardedTruffleContext ctx) {
    if (obj instanceof TruffleObject && !(obj instanceof EpbProxyValue)) {
      return new EpbProxyValue(obj, ctx);
    }
    return obj;
  }

  private static Object unwrap(Object obj, GuardedTruffleContext ctx) {
    if (obj instanceof EpbProxyValue v && v.ctx == ctx) {
      return v.value;
    } else {
      return obj;
    }
  }

  @ExportMessage
  Object send(
    Message message,
    Object[] args,
    @CachedLibrary(value = "this") ReflectionLibrary lib,
    @CachedLibrary(limit = "3") ReflectionLibrary delegate
  ) throws Exception {
    if (message.getLibraryClass() != InteropLibrary.class) {
      return delegate.send(DEFAULT, message, args);
    }
    var newArgs = new Object[args.length];
    for (int i = 0; i < args.length; i++) {
      newArgs[i] = unwrap(args[i], ctx);
    }
    var prev = ctx.enter(lib);
    try {
      var toRet = delegate.send(value, message, newArgs);
      return wrap(toRet, ctx);
    } finally {
      ctx.leave(lib, prev);
    }
  }
}
