package org.enso.profiling.sampler;

import java.time.Instant;

/** Sampler that does nothing. */
final class NoopSampler implements MethodsSampler {

  @Override
  public void start() {}

  @Override
  public void close() {}

  @Override
  public void log(Instant at, String message) {}
}
