package org.enso.interpreter.instrument.execution;

/**
 * Result of a non-blocking lock acquisition attempt.
 */
public final class TryLockResult implements AutoCloseable {
    private final boolean acquired;
    private final Runnable releaseCallback;

    public TryLockResult(boolean acquired, Runnable releaseCallback) {
        this.acquired = acquired;
        this.releaseCallback = releaseCallback;
    }

    public boolean isAcquired() {
        return acquired;
    }

    @Override
    public void close() {
        if (acquired && releaseCallback != null) {
            releaseCallback.run();
        }
    }

    public static TryLockResult notAcquired() {
        return new TryLockResult(false, null);
    }
}
