package org.enso.ydoc.server;

import java.util.function.Consumer;
import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.HostAccess;

public final class DelegateYjsChannel<M> {
  private final YjsChannel<M> delegate;

  private DelegateYjsChannel(YjsChannel<M> delegate) {
    this.delegate = delegate;
  }

  static <M> YjsChannel<M> wrap(YjsChannel<M> channel) {
    var impl = new DelegateYjsChannel<>(channel);
    var wrap = YjsChannel.create(impl::send, impl::subscribe);
    return wrap;
  }

  @HostAccess.Export
  public void send(M o) {
    Ydoc.log.trace("DelegateYjsChannel.send[{}]: {}", o.getClass(), o);
    delegate.send(o);
    Ydoc.log.trace("DelegateYjsChannel.send finished");
  }

  @SuppressWarnings("unchecked")
  @HostAccess.Export
  public void subscribe(Consumer<M> cnsmr) {
    var wrap = new DelegateConsumer(cnsmr);
    Ydoc.log.trace("DelegateYjsChannel.subscribe[{}]: {}", cnsmr.getClass(), cnsmr);
    delegate.subscribe(wrap);
    Ydoc.log.trace("DelegateYjsChannel.subscribe finished");
  }
}
