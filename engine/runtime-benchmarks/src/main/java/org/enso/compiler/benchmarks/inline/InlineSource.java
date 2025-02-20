package org.enso.compiler.benchmarks.inline;

import java.util.Set;

record InlineSource(
    String source,
    // InlineContext for the main method
    InlineContextResourceFactory inlineContextFactory,
    // Local variables in main method
    Set<String> localVarNames) {}
