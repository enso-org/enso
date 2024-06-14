package org.enso.compiler.benchmarks.inline;

import java.io.IOException;
import org.enso.compiler.context.InlineContext;

/**
 * InlineContextResource ensures that the underlying InlineContext is properly cleaned up after
 * usage.
 *
 * @param inlineContext InlineContext for the main method
 */
public record InlineContextResource(InlineContext inlineContext) implements AutoCloseable {
  @Override
  public void close() throws IOException {
    inlineContext
        .localScope()
        .foreach(
            s -> {
              s.scope().removeScopeFromParent();
              return null;
            });
  }
}
