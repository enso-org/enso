package org.enso.benchmarks.processor;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Just a dummy annotation to force the {@link BenchProcessor} to generate JMH sources
 * for all the discovered Enso benchmarks.
 */
@Retention(RetentionPolicy.CLASS)
@Target(ElementType.TYPE)
public @interface GenerateBenchSources {
}
