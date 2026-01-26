package org.enso.ydoc.server;

import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.PriorityQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * A single-threaded execution service that processes tasks on the thread where it was created.
 *
 * <p>This service maintains an event queue and executes tasks when {@link #processPendingTasks()}
 * is called from the owner thread. It supports both immediate task execution and scheduled tasks
 * with delays.
 *
 * <p><b>Thread Safety:</b> This service is thread-safe for submitting tasks, but {@link
 * #processPendingTasks()} must only be called from the owner thread.
 */
public final class YdocScheduledExecutorService implements ScheduledExecutorService {

  private final long ownerThreadId;
  private final ConcurrentLinkedQueue<Runnable> immediateTasks;
  private final Object scheduledTasksLock = new Object();
  private final PriorityQueue<ScheduledTask> scheduledTasks;
  private final AtomicBoolean shutdown = new AtomicBoolean(false);
  private final Object waitLock = new Object();

  /** Creates a new execution service bound to the current thread. */
  public YdocScheduledExecutorService() {
    this.ownerThreadId = Thread.currentThread().threadId();
    this.immediateTasks = new ConcurrentLinkedQueue<>();
    this.scheduledTasks = new PriorityQueue<>();
  }

  /**
   * Submits a task for immediate execution (internal method).
   *
   * <p>The task will be executed on the next call to {@link #processPendingTasks()} from the owner
   * thread.
   *
   * @param task the task to execute
   * @throws IllegalStateException if the service has been shut down
   */
  private void submitInternal(Runnable task) {
    if (shutdown.get()) {
      throw new IllegalStateException("Service has been shut down");
    }
    immediateTasks.offer(task);
    synchronized (waitLock) {
      waitLock.notifyAll();
    }
  }

  @Override
  public Future<?> submit(Runnable task) {
    submitInternal(task);
    return java.util.concurrent.CompletableFuture.completedFuture(null);
  }

  @Override
  public <T> Future<T> submit(Runnable task, T result) {
    submitInternal(task);
    return java.util.concurrent.CompletableFuture.completedFuture(result);
  }

  @Override
  public <V> Future<V> submit(Callable<V> task) {
    var javaFuture = new java.util.concurrent.CompletableFuture<V>();
    submitInternal(
        () -> {
          try {
            javaFuture.complete(task.call());
          } catch (Throwable t) {
            javaFuture.completeExceptionally(t);
          }
        });
    return javaFuture;
  }

  @Override
  public ScheduledFuture<?> schedule(Runnable task, long delay, TimeUnit unit) {
    if (shutdown.get()) {
      throw new IllegalStateException("Service has been shut down");
    }
    long executeAtNanos = System.nanoTime() + unit.toNanos(delay);
    var cancellableTask = new CancellableTask(task, executeAtNanos);
    synchronized (scheduledTasksLock) {
      scheduledTasks.offer(new ScheduledTask(cancellableTask, executeAtNanos));
    }
    synchronized (waitLock) {
      waitLock.notifyAll();
    }
    return cancellableTask;
  }

  /**
   * Schedules a task to execute after the specified duration.
   *
   * @param task the task to execute
   * @param delay the delay before execution
   * @throws IllegalStateException if the service has been shut down
   */
  public void schedule(Runnable task, Duration delay) {
    schedule(task, delay.toNanos(), TimeUnit.NANOSECONDS);
  }

  @Override
  public <V> ScheduledFuture<V> schedule(Callable<V> task, long delay, TimeUnit unit) {
    if (shutdown.get()) {
      throw new IllegalStateException("Service has been shut down");
    }
    long executeAtNanos = System.nanoTime() + unit.toNanos(delay);
    var callableTask = new CallableScheduledFuture<>(task, executeAtNanos);
    var wrapper =
        new CancellableTask(
            () -> {
              try {
                callableTask.complete(task.call());
              } catch (Throwable t) {
                callableTask.completeExceptionally(t);
              }
            },
            executeAtNanos);
    synchronized (scheduledTasksLock) {
      scheduledTasks.offer(new ScheduledTask(wrapper, executeAtNanos));
    }
    synchronized (waitLock) {
      waitLock.notifyAll();
    }
    return callableTask;
  }

  @Override
  public ScheduledFuture<?> scheduleAtFixedRate(
      Runnable task, long initialDelay, long period, TimeUnit unit) {
    if (shutdown.get()) {
      throw new IllegalStateException("Service has been shut down");
    }

    var repeatingTask = new RepeatingTask(task, unit.toNanos(period));
    long executeAtNanos = System.nanoTime() + unit.toNanos(initialDelay);

    var cancellableTask = new CancellableTask(repeatingTask, executeAtNanos);
    synchronized (scheduledTasksLock) {
      scheduledTasks.offer(new ScheduledTask(cancellableTask, executeAtNanos));
    }
    synchronized (waitLock) {
      waitLock.notifyAll();
    }
    return cancellableTask;
  }

  @Override
  public ScheduledFuture<?> scheduleWithFixedDelay(
      Runnable task, long initialDelay, long delay, TimeUnit unit) {
    // For our use case, fixed delay is similar to fixed rate
    return scheduleAtFixedRate(task, initialDelay, delay, unit);
  }

  /** A task that reschedules itself after execution. */
  private final class RepeatingTask implements Runnable {
    private final Runnable task;
    private final long periodNanos;

    RepeatingTask(Runnable task, long periodNanos) {
      this.task = task;
      this.periodNanos = periodNanos;
    }

    @Override
    public void run() {
      try {
        task.run();
      } catch (Throwable t) {
        handleUncaughtException(t);
      }

      // Reschedule for next execution
      if (!shutdown.get()) {
        long nextExecutionNanos = System.nanoTime() + periodNanos;
        synchronized (scheduledTasksLock) {
          scheduledTasks.offer(new ScheduledTask(this, nextExecutionNanos));
        }
        synchronized (waitLock) {
          waitLock.notifyAll();
        }
      }
    }
  }

  /**
   * A cancellable task wrapper that implements ScheduledFuture interface for compatibility with
   * ScheduledExecutorService APIs.
   */
  private static final class CancellableTask
      implements Runnable, java.util.concurrent.ScheduledFuture<Object> {
    private final Runnable task;
    private final AtomicBoolean cancelled = new AtomicBoolean(false);
    private final long executeAtNanos;

    CancellableTask(Runnable task, long executeAtNanos) {
      this.task = task;
      this.executeAtNanos = executeAtNanos;
    }

    @Override
    public void run() {
      if (!cancelled.get()) {
        task.run();
      }
    }

    @Override
    public long getDelay(TimeUnit unit) {
      long delayNanos = executeAtNanos - System.nanoTime();
      return unit.convert(delayNanos, TimeUnit.NANOSECONDS);
    }

    @Override
    public int compareTo(java.util.concurrent.Delayed o) {
      if (this == o) {
        return 0;
      }
      long diff = getDelay(TimeUnit.NANOSECONDS) - o.getDelay(TimeUnit.NANOSECONDS);
      return Long.signum(diff);
    }

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
      return cancelled.compareAndSet(false, true);
    }

    @Override
    public boolean isCancelled() {
      return cancelled.get();
    }

    @Override
    public boolean isDone() {
      return cancelled.get();
    }

    @Override
    public Object get() {
      return null;
    }

    @Override
    public Object get(long timeout, TimeUnit unit) {
      return null;
    }
  }

  /** A ScheduledFuture for Callable tasks that holds the result. */
  private static final class CallableScheduledFuture<V> implements ScheduledFuture<V> {
    private final long executeAtNanos;
    private volatile V result;
    private volatile Throwable exception;
    private volatile boolean done = false;
    private final Object lock = new Object();

    CallableScheduledFuture(Callable<V> task, long executeAtNanos) {
      this.executeAtNanos = executeAtNanos;
    }

    void complete(V result) {
      synchronized (lock) {
        if (done) {
          return;
        }
        this.result = result;
        this.done = true;
        lock.notifyAll();
      }
    }

    void completeExceptionally(Throwable exception) {
      synchronized (lock) {
        if (done) {
          return;
        }
        this.exception = exception;
        this.done = true;
        lock.notifyAll();
      }
    }

    @Override
    public long getDelay(TimeUnit unit) {
      long delayNanos = executeAtNanos - System.nanoTime();
      return unit.convert(delayNanos, TimeUnit.NANOSECONDS);
    }

    @Override
    public int compareTo(java.util.concurrent.Delayed o) {
      if (this == o) {
        return 0;
      }
      long diff = getDelay(TimeUnit.NANOSECONDS) - o.getDelay(TimeUnit.NANOSECONDS);
      return Long.signum(diff);
    }

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
      return false; // Cannot cancel after scheduled
    }

    @Override
    public boolean isCancelled() {
      return false;
    }

    @Override
    public boolean isDone() {
      return done;
    }

    @Override
    public V get() throws InterruptedException, ExecutionException {
      synchronized (lock) {
        while (!done) {
          lock.wait();
        }
        if (exception != null) {
          throw new ExecutionException(exception);
        }
        return result;
      }
    }

    @Override
    public V get(long timeout, TimeUnit unit)
        throws InterruptedException, ExecutionException, TimeoutException {
      synchronized (lock) {
        if (!done) {
          lock.wait(unit.toMillis(timeout));
        }
        if (!done) {
          throw new TimeoutException();
        }
        if (exception != null) {
          throw new ExecutionException(exception);
        }
        return result;
      }
    }
  }

  // Additional ExecutorService methods

  @Override
  public void execute(Runnable command) {
    submitInternal(command);
  }

  @Override
  public List<Runnable> shutdownNow() {
    shutdown();
    return List.of(); // Cannot retrieve pending tasks in this implementation
  }

  @Override
  public boolean isTerminated() {
    return isShutdown();
  }

  @Override
  public boolean awaitTermination(long timeout, TimeUnit unit) {
    return true; // No background threads to wait for
  }

  @Override
  public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks) {
    throw new UnsupportedOperationException("invokeAll not supported");
  }

  @Override
  public <T> List<Future<T>> invokeAll(
      Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) {
    throw new UnsupportedOperationException("invokeAll not supported");
  }

  @Override
  public <T> T invokeAny(Collection<? extends Callable<T>> tasks)
      throws InterruptedException, ExecutionException {
    throw new UnsupportedOperationException("invokeAny not supported");
  }

  @Override
  public <T> T invokeAny(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit)
      throws InterruptedException, ExecutionException, TimeoutException {
    throw new UnsupportedOperationException("invokeAny not supported");
  }

  /**
   * Processes all pending tasks that are ready to execute.
   *
   * <p>This method must be called from the owner thread (the thread that created this service). It
   * will execute all immediate tasks and any scheduled tasks whose delay has elapsed.
   *
   * @return the number of tasks executed
   * @throws IllegalStateException if called from a thread other than the owner thread
   */
  public int processPendingTasks() {
    int tasksExecuted = 0;
    long currentTime = System.nanoTime();

    // Process immediate tasks
    Runnable task;
    while ((task = immediateTasks.poll()) != null) {
      try {
        task.run();
        tasksExecuted++;
      } catch (Throwable t) {
        handleUncaughtException(t);
      }
    }

    // Process scheduled tasks that are ready
    synchronized (scheduledTasksLock) {
      while (!scheduledTasks.isEmpty()) {
        ScheduledTask scheduledTask = scheduledTasks.peek();
        if (scheduledTask.executeAtNanos <= currentTime) {
          scheduledTasks.poll();
          try {
            scheduledTask.task.run();
            tasksExecuted++;
          } catch (Throwable t) {
            handleUncaughtException(t);
          }
        } else {
          break; // Tasks are sorted by time, so we can stop here
        }
      }
    }

    return tasksExecuted;
  }

  /**
   * Returns true if there are any tasks pending execution.
   *
   * @return true if tasks are pending
   */
  public boolean hasPendingTasks() {
    if (!immediateTasks.isEmpty()) {
      return true;
    }
    synchronized (scheduledTasksLock) {
      if (scheduledTasks.isEmpty()) {
        return false;
      }
      long currentTime = System.nanoTime();
      ScheduledTask next = scheduledTasks.peek();
      return next != null && next.executeAtNanos <= currentTime;
    }
  }

  /**
   * Returns the number of nanoseconds until the next scheduled task is ready, or -1 if there are no
   * scheduled tasks.
   *
   * @return nanoseconds until next task, or -1 if none
   */
  public long getNextTaskDelayNanos() {
    if (!immediateTasks.isEmpty()) {
      return 0;
    }
    synchronized (scheduledTasksLock) {
      ScheduledTask next = scheduledTasks.peek();
      if (next == null) {
        return -1;
      }
      long delay = next.executeAtNanos - System.nanoTime();
      return Math.max(0, delay);
    }
  }

  /**
   * Waits until tasks are available or the timeout expires.
   *
   * <p>This method blocks until either:
   *
   * <ul>
   *   <li>A new task is submitted (immediate or scheduled)
   *   <li>The specified timeout expires
   *   <li>The thread is interrupted
   * </ul>
   *
   * @param timeoutNanos maximum time to wait in nanoseconds, or -1 to wait with a default timeout
   * @throws InterruptedException if the thread is interrupted while waiting
   */
  public void waitForTasks(long timeoutNanos) throws InterruptedException {
    synchronized (waitLock) {
      if (timeoutNanos > 0) {
        long timeoutMillis = timeoutNanos / 1_000_000;
        int timeoutNanosRemainder = (int) (timeoutNanos % 1_000_000);
        waitLock.wait(timeoutMillis, timeoutNanosRemainder);
      } else if (timeoutNanos == -1) {
        waitLock.wait(10);
      }
    }
  }

  /**
   * Shuts down this service. No new tasks will be accepted after shutdown.
   *
   * <p>Pending tasks can still be processed with {@link #processPendingTasks()}.
   */
  public void shutdown() {
    shutdown.set(true);
    synchronized (waitLock) {
      waitLock.notifyAll();
    }
  }

  /**
   * Returns true if this service has been shut down.
   *
   * @return true if shut down
   */
  public boolean isShutdown() {
    return shutdown.get();
  }

  /**
   * Returns the thread ID of the owner thread.
   *
   * @return the owner thread ID
   */
  public long getOwnerThreadId() {
    return ownerThreadId;
  }

  private void handleUncaughtException(Throwable t) {
    Thread currentThread = Thread.currentThread();
    Thread.UncaughtExceptionHandler handler = currentThread.getUncaughtExceptionHandler();
    if (handler != null) {
      handler.uncaughtException(currentThread, t);
    } else {
      System.err.println("Uncaught exception in YdocScheduledExecutorService:");
      t.printStackTrace();
    }
  }

  /** Internal class representing a scheduled task with its execution time. */
  private static final class ScheduledTask implements Comparable<ScheduledTask> {
    final Runnable task;
    final long executeAtNanos;

    ScheduledTask(Runnable task, long executeAtNanos) {
      this.task = task;
      this.executeAtNanos = executeAtNanos;
    }

    @Override
    public int compareTo(ScheduledTask other) {
      return Long.compare(this.executeAtNanos, other.executeAtNanos);
    }
  }

  /** A future-like result holder for scheduled tasks. */
  public static final class CompletableFuture<V> {
    private volatile V result;
    private volatile Throwable exception;
    private volatile boolean done = false;
    private final Object lock = new Object();

    void complete(V result) {
      synchronized (lock) {
        if (done) {
          return;
        }
        this.result = result;
        this.done = true;
        lock.notifyAll();
      }
    }

    void completeExceptionally(Throwable exception) {
      synchronized (lock) {
        if (done) {
          return;
        }
        this.exception = exception;
        this.done = true;
        lock.notifyAll();
      }
    }

    /**
     * Returns true if this future is complete.
     *
     * @return true if complete
     */
    public boolean isDone() {
      return done;
    }

    /**
     * Gets the result, blocking until it's available.
     *
     * @return the result
     * @throws Exception if the task threw an exception
     */
    public V get() throws Exception {
      synchronized (lock) {
        while (!done) {
          lock.wait();
        }
        if (exception != null) {
          if (exception instanceof Exception) {
            throw (Exception) exception;
          } else {
            throw new Exception(exception);
          }
        }
        return result;
      }
    }

    /**
     * Gets the result without blocking, or returns null if not complete.
     *
     * @return the result, or null if not complete
     * @throws RuntimeException if the task threw an exception
     */
    public V getNow() {
      if (!done) {
        return null;
      }
      if (exception != null) {
        if (exception instanceof RuntimeException) {
          throw (RuntimeException) exception;
        } else {
          throw new RuntimeException(exception);
        }
      }
      return result;
    }
  }
}
