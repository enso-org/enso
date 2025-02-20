package org.enso.interpreter.instrument;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.polyglot.runtime.Runtime$Api$ExecutionEnvironment;
import org.enso.polyglot.runtime.Runtime$Api$ExpressionConfig;
import scala.Option;
import scala.collection.immutable.Seq;

/**
 * The program execution config.
 *
 * @param executionEnvironment the global execution environment of the program
 * @param expressionConfigs execution configs for each expression
 */
public record ExecutionConfig(
    ExecutionEnvironment executionEnvironment, Map<UUID, ExecutionEnvironment> expressionConfigs) {

  public static ExecutionConfig empty() {
    return new ExecutionConfig(null, Collections.emptyMap());
  }

  @SuppressWarnings("unchecked")
  public static ExecutionConfig create(
      Object executionEnvironmentOption1, Object expressionConfigs1) {
    Map<UUID, ExecutionEnvironment> expressionConfigsBuilder = new HashMap<>();
    Option<Runtime$Api$ExecutionEnvironment> executionEnvironmentOption =
        (Option<Runtime$Api$ExecutionEnvironment>) executionEnvironmentOption1;
    Seq<Runtime$Api$ExpressionConfig> expressionConfigs =
        (Seq<Runtime$Api$ExpressionConfig>) expressionConfigs1;
    expressionConfigs.foreach(
        expressionConfig -> {
          expressionConfig
              .executionEnvironment()
              .foreach(
                  executionEnvironment -> {
                    expressionConfigsBuilder.put(
                        expressionConfig.expressionId(),
                        ExecutionEnvironment.forName(executionEnvironment.name()));
                    return null;
                  });
          return null;
        });

    ExecutionEnvironment executionEnvironment =
        executionEnvironmentOption
            .map(env -> ExecutionEnvironment.forName(env.name()))
            .getOrElse(() -> null);

    return new ExecutionConfig(executionEnvironment, expressionConfigsBuilder);
  }
}
