package org.enso.logging.service.common;

import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

final class LogJobsQueue {
  private final Deque<LogJob> queue = new LinkedList<>();

  /** Enqueues a log message to be sent and returns the number of messages in the queue. */
  synchronized int enqueue(LogJob message) {
    int previousSize = queue.size();
    queue.addLast(message);
    int newSize = queue.size();
    assert newSize == previousSize + 1
        : "Appending to queue is synchronized, so the size is always incremented exactly by 1.";
    return newSize;
  }

  /** Removes and returns up to {@code limit} enqueued jobs. */
  synchronized List<LogJob> popEnqueuedJobs(int limit) {
    assert limit > 0;
    if (queue.isEmpty()) {
      return List.of();
    }

    int n = Math.min(limit, queue.size());
    List<LogJob> result = new ArrayList<>(n);
    for (int i = 0; i < n; i++) {
      result.add(queue.removeFirst());
    }
    return result;
  }
}
