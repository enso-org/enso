package org.enso.base.cache;

import java.util.Optional;

public abstract class Mockable<T> {
  private Optional<T> override = Optional.empty();

  public abstract T computeValue();

  public void mocked(T t) {
    this.override = Optional.of(t);
  }

  public void unmocked() {
    this.override = Optional.empty();
  }

  public T get() {
    return override.orElse(computeValue());
  }
}
