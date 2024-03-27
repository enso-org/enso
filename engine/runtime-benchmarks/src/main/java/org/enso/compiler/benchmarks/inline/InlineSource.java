package org.enso.compiler.benchmarks.inline;

import java.util.Set;

record InlineSource(
    String source,
    // Builder for InlineContext for the main method
    InlineContextResourceBuilder builder,
    // Local variables in main method
    Set<String> localVarNames) {}
