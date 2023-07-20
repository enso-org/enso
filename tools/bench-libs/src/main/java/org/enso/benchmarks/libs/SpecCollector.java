package org.enso.benchmarks.libs;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Objects;
import org.enso.polyglot.MethodNames.Module;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;

/**
 * Collect benchmark specifications from source files.
 */
public class SpecCollector {
  private final Context ctx;

  public SpecCollector(Context ctx) {
    this.ctx = ctx;
  }

  public Collection<BenchSuite> collectBenchSpecsFromDir(File rootDir) {
    throw new UnsupportedOperationException("unimplemented");
  }

  /**
   *
   * @param benchFile
   * @return null if there are no benchmark specs in the file.
   */
  public BenchSuite collectBenchSpecsFromSingleFile(File benchFile) {
    Objects.requireNonNull(benchFile);
    assert benchFile.exists();
    assert benchFile.canRead();
    Source source;
    try {
      source = Source.newBuilder("enso", benchFile).build();
    } catch (IOException e) {
      throw new IllegalStateException("Unreachable", e);
    }
    Value module = ctx.eval(source);
    BenchSuite benchSuite = null;
    Value associatedType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    // TODO: Try to
    try {
      benchSuite = module
          .invokeMember(Module.EVAL_EXPRESSION, "all")
          .as(BenchSuite.class);
    } catch (PolyglotException e) {
      // TODO: Handle this properly
      throw new IllegalStateException("Unreachable", e);
    }
    return benchSuite;
  }
}
