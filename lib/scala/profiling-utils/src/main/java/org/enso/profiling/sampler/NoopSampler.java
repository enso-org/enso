package org.enso.profiling.sampler;

import org.enso.profiling.sampler.MethodsSampler;

/** Sampler that does nothing. */
public class NoopSampler implements MethodsSampler {

  @Override
  public void start() {}

  @Override
  public void stop() {}
}
