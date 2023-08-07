package org.enso.benchmarks.processor;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Use this annotation to force the {@link BenchProcessor} to generate JMH sources
 * for all the collected Enso benchmarks. The location of the benchmarks is encoded
 * by the {@code projectRootPath}, {@code moduleName} and {@code variableName} parameters.
 */
@Retention(RetentionPolicy.CLASS)
@Target(ElementType.TYPE)
public @interface GenerateBenchSources {

  /**
   * Path to the project root directory. Relative to the Enso repository root.
   */
  String projectRootPath();

  /**
   * Fully qualified name of the module within the project that defines all the benchmark {@link org.enso.benchmarks.BenchSuite suites}.
   * For example {@code local.Benchmarks.Main}.
   */
  String moduleName();

  /**
   * Name of the variable that holds a list of all the benchmark {@link org.enso.benchmarks.BenchSuite suites}.
   */
  String variableName() default "all_benchmarks";
}
