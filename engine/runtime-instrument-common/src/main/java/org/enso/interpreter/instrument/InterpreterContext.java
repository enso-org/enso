package org.enso.interpreter.instrument;

import com.oracle.truffle.api.TruffleContext;
import java.util.concurrent.ExecutorService;
import org.enso.interpreter.service.ExecutionService;

/**
 * Contains suppliers of services that provide interpreter specific functionality.
 *
 * @param executionService a service allowing externally-triggered code execution
 * @param contextManager a storage for active execution contexts
 * @param endpoint a message endpoint
 * @param truffleContext a context of a set of Truffle languages
 */
public record InterpreterContext(
    ExecutionService executionService,
    ExecutorService jobExecutor,
    ExecutionContextManager contextManager,
    Endpoint endpoint,
    TruffleContext truffleContext) {}
