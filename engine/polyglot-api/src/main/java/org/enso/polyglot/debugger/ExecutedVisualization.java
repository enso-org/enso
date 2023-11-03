package org.enso.polyglot.debugger;

import java.util.UUID;

public record ExecutedVisualization(
    Object result,
    Throwable error,
    UUID visualizationId,
    UUID expressionId,
    Object expressionValue) {}
