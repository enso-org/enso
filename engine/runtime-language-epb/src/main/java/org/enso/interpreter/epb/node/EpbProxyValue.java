package org.enso.interpreter.epb.node;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.library.ReflectionLibrary;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;

@ExportLibrary(value = ReflectionLibrary.class)
final class EpbProxyValue implements TruffleObject {

  private final GuardedTruffleContext ctx;
  private final Object value;

  EpbProxyValue(Object v, GuardedTruffleContext ctx) {
    this.value = v;
    this.ctx = ctx;
  }

  @ExportMessage
  Object send(
      Message message,
      Object[] args,
      @CachedLibrary(value = "this") ReflectionLibrary lib,
      @CachedLibrary(limit = "3") ReflectionLibrary delegate)
      throws Exception {
    java.lang.Object prev = ctx.enter(lib);
    try {
      return delegate.send(value, message, args);
    } finally {
      ctx.leave(lib, prev);
    }
  }
}
