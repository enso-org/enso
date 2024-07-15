package org.enso.interpreter.instrument.execution;

import java.util.UUID;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Wrapper around a lock used for Context access.
 *
 * @param lock reentrant lock unique for UUID
 * @param uuid UUID this lock is assigned to
 */
public record ContextLock(ReentrantLock lock, UUID uuid) {}
