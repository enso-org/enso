package org.enso.ydoc.server;

import org.enso.ydoc.api.YjsChannel;
import org.graalvm.polyglot.HostAccess;

public final class DelegateYjsChannelCallbacks<M> implements YjsChannel.Server<M> {
  private final String name;
  private final YjsChannel.Server<M> delegate;

  DelegateYjsChannelCallbacks(String name, YjsChannel.Server<M> delegate) {
    this.name = name;
    this.delegate = delegate;
  }

  @HostAccess.Export
  @Override
  public void onConnect(YjsChannel<M> channel) {
    Ydoc.log.trace("Enter onConnect[{}] with {} for {}", name, channel, delegate);
    if (delegate != null) {
      var wrap = DelegateYjsChannel.wrap(channel);
      delegate.onConnect(wrap);
    }
    Ydoc.log.trace("Exit onConnect[{}] with {}", name, channel);
  }
}
