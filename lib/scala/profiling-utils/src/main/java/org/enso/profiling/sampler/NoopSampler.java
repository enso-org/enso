package org.enso.profiling.sampler;

/** Sampler that does nothing. */
public class NoopSampler implements MethodsSampler {

  @Override
  public void start() {}

  @Override
  public void stop() {}
}
