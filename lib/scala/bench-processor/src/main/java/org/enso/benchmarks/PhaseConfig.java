package org.enso.benchmarks;

/**
 * Configuration for one phase - either warmup or measure.
 * Corresponds to {@code Phase_Conf} in {@code distribution/lib/Standard/Test/0.0.0-dev/src/Bench.enso}.
 */
public interface PhaseConfig {

  /**
   * Number of iterations to run the phase for.
   */
  long iterations();

  /**
   * Number of minimal amount of seconds per iterations.
   */
  long seconds();
}
