package org.enso.ydoc.server;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.Test;

public class YdocScheduledExecutorServiceTest {

  @Test
  public void testImmediateTaskExecution() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Submit immediate tasks
    service.submit(() -> counter.incrementAndGet());
    service.submit(() -> counter.incrementAndGet());
    service.submit(() -> counter.incrementAndGet());

    // Process tasks
    int executed = service.processPendingTasks();

    assertEquals(3, executed);
    assertEquals(3, counter.get());
  }

  @Test
  public void testTaskExecutionOrder() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    List<Integer> executionOrder = new ArrayList<>();

    // Submit tasks in order
    service.submit(() -> executionOrder.add(1));
    service.submit(() -> executionOrder.add(2));
    service.submit(() -> executionOrder.add(3));

    service.processPendingTasks();

    // Verify FIFO order
    assertEquals(List.of(1, 2, 3), executionOrder);
  }

  @Test
  public void testScheduledTaskWithDelay() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Schedule a task with delay
    service.schedule(() -> counter.incrementAndGet(), 50, TimeUnit.MILLISECONDS);

    // Process immediately - should not execute yet
    int executed1 = service.processPendingTasks();
    assertEquals(0, executed1);
    assertEquals(0, counter.get());

    // Wait for the delay
    Thread.sleep(60);

    // Process again - should execute now
    int executed2 = service.processPendingTasks();
    assertEquals(1, executed2);
    assertEquals(1, counter.get());
  }

  @Test
  public void testMultipleScheduledTasks() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    List<Integer> executionOrder = new ArrayList<>();

    // Schedule tasks with different delays
    service.schedule(() -> executionOrder.add(3), 60, TimeUnit.MILLISECONDS);
    service.schedule(() -> executionOrder.add(1), 20, TimeUnit.MILLISECONDS);
    service.schedule(() -> executionOrder.add(2), 40, TimeUnit.MILLISECONDS);

    // Wait for all delays to pass
    Thread.sleep(70);

    // Process - should execute in order of delay
    service.processPendingTasks();

    assertEquals(List.of(1, 2, 3), executionOrder);
  }

  @Test
  public void testMixedImmediateAndScheduledTasks() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    List<String> executionOrder = new ArrayList<>();

    // Mix immediate and scheduled tasks
    service.submit(() -> executionOrder.add("immediate1"));
    service.schedule(() -> executionOrder.add("scheduled1"), 30, TimeUnit.MILLISECONDS);
    service.submit(() -> executionOrder.add("immediate2"));

    // Process immediate tasks first
    service.processPendingTasks();
    assertEquals(List.of("immediate1", "immediate2"), executionOrder);

    // Wait and process scheduled task
    Thread.sleep(40);
    service.processPendingTasks();
    assertEquals(List.of("immediate1", "immediate2", "scheduled1"), executionOrder);
  }

  @Test
  public void testCallableWithResult() throws Exception {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    var future = service.submit(() -> "Hello World");

    service.processPendingTasks();

    assertTrue(future.isDone());
    assertEquals("Hello World", future.get());
  }

  @Test
  public void testCallableWithException() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    var future =
        service.submit(
            () -> {
              throw new RuntimeException("Test exception");
            });

    service.processPendingTasks();

    assertTrue(future.isDone());
    try {
      future.get();
      fail("Expected exception");
    } catch (Exception e) {
      assertTrue(e.getMessage().contains("Test exception"));
    }
  }

  @Test
  public void testScheduledCallable() throws Exception {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    var future = service.schedule(() -> 42, 30, TimeUnit.MILLISECONDS);

    assertFalse(future.isDone());

    Thread.sleep(40);
    service.processPendingTasks();

    assertTrue(future.isDone());
    assertEquals(Integer.valueOf(42), future.get());
  }

  @Test
  public void testTasksExecuteOnOwnerThread() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    long ownerThreadId = Thread.currentThread().threadId();

    List<Long> immediateTaskThreadIds = new ArrayList<>();
    List<Long> scheduledTaskThreadIds = new ArrayList<>();

    // Submit immediate tasks from different threads
    service.submit(() -> immediateTaskThreadIds.add(Thread.currentThread().threadId()));

    Thread submitterThread =
        new Thread(
            () -> {
              service.submit(() -> immediateTaskThreadIds.add(Thread.currentThread().threadId()));
              service.schedule(
                  () -> scheduledTaskThreadIds.add(Thread.currentThread().threadId()),
                  30,
                  TimeUnit.MILLISECONDS);
            });
    submitterThread.start();
    submitterThread.join();

    // Schedule a task from the owner thread
    service.schedule(
        () -> scheduledTaskThreadIds.add(Thread.currentThread().threadId()),
        50,
        TimeUnit.MILLISECONDS);

    // Process immediate tasks on owner thread
    service.processPendingTasks();

    // All immediate tasks should have executed on owner thread
    assertEquals(2, immediateTaskThreadIds.size());
    for (Long threadId : immediateTaskThreadIds) {
      assertEquals(ownerThreadId, threadId.longValue());
    }

    // Wait for scheduled tasks
    Thread.sleep(60);
    service.processPendingTasks();

    // All scheduled tasks should have executed on owner thread
    assertEquals(2, scheduledTaskThreadIds.size());
    for (Long threadId : scheduledTaskThreadIds) {
      assertEquals(ownerThreadId, threadId.longValue());
    }
  }

  @Test
  public void testHasPendingTasks() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    assertFalse(service.hasPendingTasks());

    service.submit(() -> {});
    assertTrue(service.hasPendingTasks());

    service.processPendingTasks();
    assertFalse(service.hasPendingTasks());

    service.schedule(() -> {}, 30, TimeUnit.MILLISECONDS);
    assertFalse(service.hasPendingTasks()); // Not ready yet

    Thread.sleep(40);
    assertTrue(service.hasPendingTasks()); // Now ready
  }

  @Test
  public void testGetNextTaskDelay() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    assertEquals(-1, service.getNextTaskDelayNanos()); // No tasks

    service.submit(() -> {});
    assertEquals(0, service.getNextTaskDelayNanos()); // Immediate task

    service.processPendingTasks();
    assertEquals(-1, service.getNextTaskDelayNanos()); // No tasks again

    service.schedule(() -> {}, 100, TimeUnit.MILLISECONDS);
    long delay = service.getNextTaskDelayNanos();
    assertTrue(delay > 0 && delay <= TimeUnit.MILLISECONDS.toNanos(100));
  }

  @Test
  public void testShutdown() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();

    assertFalse(service.isShutdown());

    service.shutdown();
    assertTrue(service.isShutdown());

    // Should not accept new tasks after shutdown
    try {
      service.submit(() -> {});
      fail("Expected IllegalStateException");
    } catch (IllegalStateException e) {
      // Expected
    }
  }

  @Test
  public void testExceptionHandlingInTask() {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Submit task that throws exception
    service.submit(
        () -> {
          throw new RuntimeException("Test exception");
        });

    // Submit another task to verify service continues
    service.submit(() -> counter.incrementAndGet());

    // Process tasks - should handle exception and continue
    service.processPendingTasks();

    // Second task should have executed despite first one throwing
    assertEquals(1, counter.get());
  }

  @Test
  public void testEventLoopPattern() throws InterruptedException {
    YdocScheduledExecutorService service = new YdocScheduledExecutorService();
    AtomicInteger counter = new AtomicInteger(0);

    // Schedule recurring tasks
    service.submit(() -> counter.incrementAndGet());
    service.schedule(() -> counter.incrementAndGet(), 10, TimeUnit.MILLISECONDS);
    service.schedule(() -> counter.incrementAndGet(), 20, TimeUnit.MILLISECONDS);

    // Simulate event loop
    for (int i = 0; i < 5; i++) {
      service.processPendingTasks();
      Thread.sleep(10);
    }

    // All tasks should have executed
    assertEquals(3, counter.get());
  }
}
