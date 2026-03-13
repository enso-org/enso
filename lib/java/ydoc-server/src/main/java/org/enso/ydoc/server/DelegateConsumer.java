package org.enso.ydoc.server;

import java.util.function.Consumer;
import org.graalvm.polyglot.HostAccess;

public final class DelegateConsumer<T> implements Consumer<T> {

  private final Consumer<T> delegate;

  DelegateConsumer(Consumer<T> delegate) {
    this.delegate = delegate;
  }

  @Override
  @HostAccess.Export
  public void accept(T t) {
    Ydoc.log.trace("DelegateConsumer.accept[{}]: {}", t.getClass(), t);
    delegate.accept(t);
    Ydoc.log.trace("DelegateConsumer.accept finished");
  }
}
