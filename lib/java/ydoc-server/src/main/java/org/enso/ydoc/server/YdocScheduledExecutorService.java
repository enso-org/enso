package org.enso.ydoc.server;

import java.time.Duration;
import java.util.Collection;
import java.util.List;
import java.util.PriorityQueue;
import java.util.concurrent.Callable;
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
final class YdocScheduledExecutorService implements ScheduledExecutorService {

  /**
   * A marker wrapper that signals a task should be placed in the high-priority queue. Tasks wrapped
   * in this class are processed before regular immediate tasks, ensuring that latency-sensitive
   * operations (such as WebSocket message handling) are not starved by large batches of bulk work.
   *
   * <p>This class is intentionally simple and decoupled from any specific module — callers wrap
   * tasks in {@code HighPriorityRunnable} before submitting them via the standard {@link
   * ScheduledExecutorService} interface, so the submitting module does not need to depend on this
   * executor implementation.
   */
  public static final class HighPriorityRunnable implements Runnable {
    private final Runnable delegate;

    public HighPriorityRunnable(Runnable delegate) {
      this.delegate = delegate;
    }

    @Override
    public void run() {
      delegate.run();
    }
  }

  private final long ownerThreadId;
  private final Object lock = new Object();
  private final boolean debug;

  /**
   * @GuardedBy("lock")
   */
  private final java.util.LinkedList<Runnable> highPriorityTasks;

  /**
   * @GuardedBy("lock")
   */
  private final java.util.LinkedList<Runnable> immediateTasks;

  /**
   * @GuardedBy("lock")
   */
  private final PriorityQueue<ScheduledTask> scheduledTasks;

  /**
   * @GuardedBy("lock")
   */
  private boolean shutdown = false;

  private final TaskStats highPriorityStats;
  private final TaskStats regularStats;

  /** Creates a new execution service bound to the current thread with debug disabled. */
  public YdocScheduledExecutorService() {
    this(false);
  }

  /**
   * Creates a new execution service bound to the current thread.
   *
   * @param debug when true, enables per-task execution timing statistics
   */
  public YdocScheduledExecutorService(boolean debug) {
    this.ownerThreadId = Thread.currentThread().threadId();
    this.highPriorityTasks = new java.util.LinkedList<>();
    this.immediateTasks = new java.util.LinkedList<>();
    this.scheduledTasks = new PriorityQueue<>();
    this.debug = debug;
    this.highPriorityStats = debug ? new TaskStats() : null;
    this.regularStats = debug ? new TaskStats() : null;
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
    synchronized (lock) {
      if (shutdown) {
        throw new IllegalStateException("Service has been shut down");
      }
      immediateTasks.offer(task);
      lock.notifyAll();
    }
  }

  private void submitHighPriority(Runnable task) {
    synchronized (lock) {
      if (shutdown) {
        throw new IllegalStateException("Service has been shut down");
      }
      highPriorityTasks.offer(task);
      lock.notifyAll();
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
    synchronized (lock) {
      if (shutdown) {
        throw new IllegalStateException("Service has been shut down");
      }
      long executeAtNanos = System.nanoTime() + unit.toNanos(delay);
      var cancellableTask = new CancellableTask(task, executeAtNanos);
      scheduledTasks.offer(new ScheduledTask(cancellableTask, executeAtNanos));
      lock.notifyAll();
      return cancellableTask;
    }
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
    synchronized (lock) {
      if (shutdown) {
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
      scheduledTasks.offer(new ScheduledTask(wrapper, executeAtNanos));
      lock.notifyAll();
      return callableTask;
    }
  }

  @Override
  public ScheduledFuture<?> scheduleAtFixedRate(
      Runnable task, long initialDelay, long period, TimeUnit unit) {
    synchronized (lock) {
      if (shutdown) {
        throw new IllegalStateException("Service has been shut down");
      }

      var repeatingTask = new RepeatingTask(task, unit.toNanos(period));
      long executeAtNanos = System.nanoTime() + unit.toNanos(initialDelay);

      var cancellableTask = new CancellableTask(repeatingTask, executeAtNanos);
      scheduledTasks.offer(new ScheduledTask(cancellableTask, executeAtNanos));
      lock.notifyAll();
      return cancellableTask;
    }
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
      synchronized (lock) {
        if (!shutdown) {
          long nextExecutionNanos = System.nanoTime() + periodNanos;
          scheduledTasks.offer(new ScheduledTask(this, nextExecutionNanos));
          lock.notifyAll();
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
      if (o instanceof CancellableTask other) {
        return Long.compare(this.executeAtNanos, other.executeAtNanos);
      }
      return Long.signum(getDelay(TimeUnit.NANOSECONDS) - o.getDelay(TimeUnit.NANOSECONDS));
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
    private final Object lock = new Object();

    /**
     * @GuardedBy("lock")
     */
    private V result;

    /**
     * @GuardedBy("lock")
     */
    private Throwable exception;

    /**
     * @GuardedBy("lock")
     */
    private boolean done = false;

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
      if (o instanceof CallableScheduledFuture<?> other) {
        return Long.compare(this.executeAtNanos, other.executeAtNanos);
      }
      return Long.signum(getDelay(TimeUnit.NANOSECONDS) - o.getDelay(TimeUnit.NANOSECONDS));
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
      synchronized (lock) {
        return done;
      }
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
    if (command instanceof HighPriorityRunnable) {
      submitHighPriority(command);
    } else {
      submitInternal(command);
    }
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
   * Processes pending tasks that are ready to execute, one at a time.
   *
   * <p>This method must be called from the owner thread (the thread that created this service).
   * Tasks are polled individually from the queues on each iteration so that tasks submitted during
   * execution (e.g., incoming WebSocket messages) become visible immediately. High-priority tasks
   * are always checked first, ensuring latency-sensitive operations are not starved by large
   * batches of bulk work.
   *
   * <p>Poll order: high-priority queue → regular immediate queue → ready scheduled tasks.
   *
   * @return the number of tasks executed
   * @throws IllegalStateException if called from a thread other than the owner thread
   */
  public int processPendingTasks() {
    int tasksExecuted = 0;

    while (true) {
      // Poll one task at a time, checking high-priority queue first
      Runnable task;
      int source = 0; // 0=none, 1=high-priority, 2=regular
      synchronized (lock) {
        task = highPriorityTasks.poll();
        if (task != null) {
          source = 1;
        } else {
          task = immediateTasks.poll();
          if (task != null) {
            source = 2;
          } else {
            ScheduledTask scheduledTask = scheduledTasks.peek();
            if (scheduledTask != null && scheduledTask.executeAtNanos <= System.nanoTime()) {
              scheduledTasks.poll();
              task = scheduledTask.task;
              source = 2;
            }
          }
        }
      }

      if (task == null) {
        break; // No more ready tasks
      }

      long startNanos = debug ? System.nanoTime() : 0;
      try {
        task.run();
        tasksExecuted++;
      } catch (Throwable t) {
        handleUncaughtException(t);
      }
      if (debug) {
        long elapsed = System.nanoTime() - startNanos;
        (source == 1 ? highPriorityStats : regularStats).record(elapsed);
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
    synchronized (lock) {
      if (!highPriorityTasks.isEmpty() || !immediateTasks.isEmpty()) {
        return true;
      }
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
    synchronized (lock) {
      if (!highPriorityTasks.isEmpty() || !immediateTasks.isEmpty()) {
        return 0;
      }
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
    synchronized (lock) {
      if (timeoutNanos > 0) {
        long timeoutMillis = timeoutNanos / 1_000_000;
        int timeoutNanosRemainder = (int) (timeoutNanos % 1_000_000);
        lock.wait(timeoutMillis, timeoutNanosRemainder);
      } else if (timeoutNanos == -1) {
        lock.wait(10);
      }
    }
  }

  /**
   * Returns a {@link ScheduledExecutorService} view of this executor where all {@code execute} and
   * {@code submit} calls route tasks to the high-priority queue. Scheduled tasks ({@code schedule},
   * {@code scheduleAtFixedRate}, etc.) are delegated unchanged since they fire at their scheduled
   * time and are not subject to queue starvation.
   *
   * <p>Use this to wrap the executor passed to latency-sensitive subsystems (e.g., WebSocket
   * polyfill) without introducing a module dependency on this class.
   */
  public ScheduledExecutorService createHighPriorityView() {
    return new HighPriorityExecutorView(this);
  }

  private static final class HighPriorityExecutorView implements ScheduledExecutorService {
    private final YdocScheduledExecutorService delegate;

    HighPriorityExecutorView(YdocScheduledExecutorService delegate) {
      this.delegate = delegate;
    }

    @Override
    public void execute(Runnable command) {
      delegate.execute(new HighPriorityRunnable(command));
    }

    @Override
    public Future<?> submit(Runnable task) {
      return delegate.submit(new HighPriorityRunnable(task));
    }

    @Override
    public <T> Future<T> submit(Runnable task, T result) {
      return delegate.submit(new HighPriorityRunnable(task), result);
    }

    @Override
    public <V> Future<V> submit(Callable<V> task) {
      var future = new java.util.concurrent.CompletableFuture<V>();
      delegate.execute(
          new HighPriorityRunnable(
              () -> {
                try {
                  future.complete(task.call());
                } catch (Throwable t) {
                  future.completeExceptionally(t);
                }
              }));
      return future;
    }

    @Override
    public ScheduledFuture<?> schedule(Runnable task, long delay, TimeUnit unit) {
      return delegate.schedule(task, delay, unit);
    }

    @Override
    public <V> ScheduledFuture<V> schedule(Callable<V> task, long delay, TimeUnit unit) {
      return delegate.schedule(task, delay, unit);
    }

    @Override
    public ScheduledFuture<?> scheduleAtFixedRate(
        Runnable task, long initialDelay, long period, TimeUnit unit) {
      return delegate.scheduleAtFixedRate(task, initialDelay, period, unit);
    }

    @Override
    public ScheduledFuture<?> scheduleWithFixedDelay(
        Runnable task, long initialDelay, long delay, TimeUnit unit) {
      return delegate.scheduleWithFixedDelay(task, initialDelay, delay, unit);
    }

    @Override
    public void shutdown() {
      delegate.shutdown();
    }

    @Override
    public List<Runnable> shutdownNow() {
      return delegate.shutdownNow();
    }

    @Override
    public boolean isShutdown() {
      return delegate.isShutdown();
    }

    @Override
    public boolean isTerminated() {
      return delegate.isTerminated();
    }

    @Override
    public boolean awaitTermination(long timeout, TimeUnit unit) {
      return delegate.awaitTermination(timeout, unit);
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
  }

  /**
   * Shuts down this service. No new tasks will be accepted after shutdown.
   *
   * <p>Pending tasks can still be processed with {@link #processPendingTasks()}.
   */
  public void shutdown() {
    synchronized (lock) {
      shutdown = true;
      lock.notifyAll();
    }
  }

  /**
   * Returns true if this service has been shut down.
   *
   * @return true if shut down
   */
  public boolean isShutdown() {
    synchronized (lock) {
      return shutdown;
    }
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

  /**
   * Returns a formatted summary of task execution statistics, or an empty string when debug is off
   * or no tasks have been recorded.
   */
  public String getDebugStats() {
    if (!debug) {
      return "";
    }
    if (highPriorityStats.count == 0 && regularStats.count == 0) {
      return "";
    }
    var sb = new StringBuilder();
    sb.append("Task execution stats:\n");
    sb.append("  High priority: ").append(highPriorityStats.format()).append('\n');
    sb.append("  Regular:       ").append(regularStats.format());
    return sb.toString();
  }

  /** Accumulates per-task execution timing. Only accessed from the owner thread. */
  static final class TaskStats {
    long count;
    long totalNanos;
    long minNanos = Long.MAX_VALUE;
    long maxNanos = Long.MIN_VALUE;

    void record(long elapsedNanos) {
      count++;
      totalNanos += elapsedNanos;
      if (elapsedNanos < minNanos) minNanos = elapsedNanos;
      if (elapsedNanos > maxNanos) maxNanos = elapsedNanos;
    }

    String format() {
      if (count == 0) {
        return "count=0";
      }
      double totalMs = totalNanos / 1_000_000.0;
      double minMs = minNanos / 1_000_000.0;
      double maxMs = maxNanos / 1_000_000.0;
      double avgMs = totalMs / count;
      return String.format(
          "count=%d, total=%.1fms, min=%.2fms, max=%.2fms, avg=%.2fms",
          count, totalMs, minMs, maxMs, avgMs);
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
}
