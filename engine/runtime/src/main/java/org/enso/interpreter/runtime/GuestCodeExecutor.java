package org.enso.interpreter.runtime;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

abstract class GuestCodeExecutor implements ScheduledExecutorService {
  final ScheduledExecutorService guestCode;

  GuestCodeExecutor(ScheduledExecutorService delegate) {
    this.guestCode = delegate;
  }

  protected abstract Runnable wrap(Runnable r);

  protected abstract <V> Callable<V> wrap(Callable<V> c);

  private <T> List<Callable<T>> wrap(Collection<? extends Callable<T>> tasks) {
    return tasks.stream().map(this::wrap).toList();
  }

  @Override
  public final ScheduledFuture<?> schedule(Runnable r, long l, TimeUnit tu) {
    return guestCode.schedule(wrap(r), l, tu);
  }

  @Override
  public final <V> ScheduledFuture<V> schedule(Callable<V> clbl, long l, TimeUnit tu) {
    return guestCode.schedule(wrap(clbl), l, tu);
  }

  @Override
  public final ScheduledFuture<?> scheduleAtFixedRate(Runnable r, long l, long l1, TimeUnit tu) {
    return guestCode.scheduleAtFixedRate(wrap(r), l, l1, tu);
  }

  @Override
  public final ScheduledFuture<?> scheduleWithFixedDelay(Runnable r, long l, long l1, TimeUnit tu) {
    return guestCode.scheduleWithFixedDelay(wrap(r), l, l1, tu);
  }

  @Override
  public final List<Runnable> shutdownNow() {
    return guestCode.shutdownNow();
  }

  @Override
  public final boolean isShutdown() {
    return guestCode.isShutdown();
  }

  @Override
  public final boolean isTerminated() {
    return guestCode.isTerminated();
  }

  @Override
  public final boolean awaitTermination(long l, TimeUnit tu) throws InterruptedException {
    return guestCode.awaitTermination(l, tu);
  }

  @Override
  public final <T> Future<T> submit(Callable<T> clbl) {
    return guestCode.submit(wrap(clbl));
  }

  @Override
  public final <T> Future<T> submit(Runnable r, T t) {
    return guestCode.submit(wrap(r), t);
  }

  @Override
  public final Future<?> submit(Runnable r) {
    return guestCode.submit(r);
  }

  @Override
  public final <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks)
      throws InterruptedException {
    return guestCode.invokeAll(wrap(tasks));
  }

  @Override
  public final <T> List<Future<T>> invokeAll(
      Collection<? extends Callable<T>> clctn, long l, TimeUnit tu) throws InterruptedException {
    return guestCode.invokeAll(wrap(clctn), l, tu);
  }

  @Override
  public final <T> T invokeAny(Collection<? extends Callable<T>> tasks)
      throws InterruptedException, ExecutionException {
    return guestCode.invokeAny(wrap(tasks));
  }

  @Override
  public final <T> T invokeAny(Collection<? extends Callable<T>> clctn, long l, TimeUnit tu)
      throws InterruptedException, ExecutionException, TimeoutException {
    return guestCode.invokeAny(wrap(clctn), l, tu);
  }

  @Override
  public final void execute(Runnable command) {
    guestCode.execute(wrap(command));
  }
}
