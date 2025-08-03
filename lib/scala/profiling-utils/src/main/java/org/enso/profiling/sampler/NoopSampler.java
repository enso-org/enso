package org.enso.profiling.sampler;

/** Sampler that does nothing. */
final class NoopSampler implements MethodsSampler {

  @Override
  public void start() {}

  @Override
  public void close() {}

  @Override
  public void log(String message) {}
}
