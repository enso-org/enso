package org.enso.compiler.benchmarks.inline;

import java.util.Set;
import org.enso.compiler.context.InlineContext;

record InlineSource(
    String source,
    // InlineContext for the main method
    InlineContext mainInlineContext,
    // Local variables in main method
    Set<String> localVarNames
) {}
