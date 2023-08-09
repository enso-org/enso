package org.enso.benchmarks;

/**
 * Configuration for one phase - either warmup or measure.
 * Corresponds to {@code Phase_Conf} in {@code distribution/lib/Standard/Test/0.0.0-dev/src/Bench.enso}.
 */
public interface PhaseConfig {

  /**
   * Number of seconds to run the phase for.
   * @return null if only iterations are configured.
   */
  Integer seconds();

  /**
   * Number of iterations to run the phase for.
   * @return null if only seconds are configured.
   */
  Integer iterations();
}
