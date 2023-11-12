package org.enso.profiling;

/** Sampler that does nothing. */
public class NoopSampler implements MethodsSampler {

  @Override
  public void start() {}

  @Override
  public void stop() {}
}
