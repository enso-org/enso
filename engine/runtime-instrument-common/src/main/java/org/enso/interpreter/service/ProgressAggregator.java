package org.enso.interpreter.service;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.function.BiConsumer;

/**
 * Example of a progress aggregator able to nest multiple Progress and compute % of aggregated
 * progress.
 */
final class ProgressAggregator {
  /**
   * @GuardedBy("this")
   */
  private final Map<Object, Progress> map = new WeakHashMap<>();

  private final BiConsumer<Double, String> updateStatus;
  private final Deque<Progress> stack = new ArrayDeque<>();
  private double current;
  private String message;

  /**
   * New aggregator with associated callback.
   *
   * @param updateStatus called whenever percentage of the aggregated progress updates (is
   *     increased)
   */
  public ProgressAggregator(BiConsumer<Double, String> updateStatus) {
    this.updateStatus = updateStatus;
  }

  /**
   * Starts new progress in the current aggregator's stack.Nests the current progress in the
   * currently executing step of current progress.
   *
   * @param key
   * @param max maximum number of steps the progress can "advance to"
   */
  public synchronized void create(Object key, long max) {
    Progress p;
    if (stack.isEmpty()) {
      p = new Progress(max, 0.0, 1.0);
    } else {
      var previous = stack.peek();
      var current = previous.from + previous.singleStep() * previous.current;
      p = new Progress(max, current, current + previous.singleStep());
    }
    stack.addFirst(p);
    map.put(key, p);
  }

  public void closeProgress(Object key) {
    if (findBy(key) instanceof Progress p) {
      p.advance(p.max);
      stack.remove(p);
    }
  }

  public void advanceBy(Object key, long steps) {
    if (findBy(key) instanceof Progress p) {
      p.advance(steps);
    }
  }

  public void log(Object key, String msg) {
    if (findBy(key) instanceof Progress p) {
      message = msg;
      updateStatus.accept(this.current, msg);
    }
  }

  private synchronized Progress findBy(Object key) {
    return map.get(key);
  }

  private void advanceTo(double now) {
    var notify = false;
    assert now >= 0.0 && now <= 1.0;
    synchronized (this) {
      if (now > this.current) {
        this.current = now;
        notify = true;
      }
    }
    if (notify) {
      assert !Thread.holdsLock(this);
      updateStatus.accept(now, message);
    }
  }

  final class Progress implements AutoCloseable {
    private final long max;
    private long current;
    private final double from;
    private final double to;

    private Progress(long max, double from, double to) {
      assert 0.0 <= from && from <= 1.0;
      assert 0.0 <= to && to <= 1.0;
      assert from <= to;

      if (max < 1) {
        max = 1;
      }
      this.max = max;
      this.from = from;
      this.to = to;
    }

    private final double singleStep() {
      return (to - from) / max;
    }

    /**
     * Moves this progress forward by specified number of steps. If the steps get <em>"too far"</em>
     * with respect to associated {@code max} number of steps, then the state is rounded to the
     * maximum.
     */
    public final void advance(long steps) {
      if (steps < 0) {
        steps = 0;
      }
      try {
        this.current = Math.min(Math.addExact(this.current, steps), this.max);
        advanceTo(from + this.current * singleStep());
      } catch (ArithmeticException e) {
        // keep unchanged
      }
    }

    /**
     * Closes the progress and removes it from the stack of currently active progresses. Most
     * importantly, no new progress will become child of this progress anymore.
     */
    @Override
    public void close() {
      advance(max);
      closeProgress(this);
    }
  }
}
