package org.enso.interpreter.runtime.tag;

import com.oracle.truffle.api.instrumentation.Tag;

/**
 * Marks nodes that shall not be instrumented by the IDE. Apply to {@link
 * com.oracle.truffle.api.nodes.Node nodes} that shall not be instrumented by {@link
 * IdExecutionService} provided by {@code IdExecutionInstrument}.
 */
public final class AvoidIdInstrumentationTag extends Tag {}
