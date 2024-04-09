package org.enso.polyglot.debugger;

import java.util.UUID;

/**
 * The result of executed oneshot visualization expression.
 *
 * @param result the execution result. {@code null} if the execution resulted in exception.
 * @param error the execution error. {@code null} if the execution was successful.
 * @param visualizationId the visualization id.
 * @param expressionId the id of expression that provides the execution scope.
 * @param expressionValue the value of the expression that provides the execution scope.
 */
public record ExecutedVisualization(
    Object result,
    Throwable error,
    UUID visualizationId,
    UUID expressionId,
    Object expressionValue) {}
