package org.enso.base.enso_cloud.audit;

import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

class LogJobsQueue {

  private final Deque<LogJob> queue = new LinkedList<>();

  /**
   * Enqueues a log message to be sent and returns the number of messages in the queue.
   */
  synchronized int enqueue(LogJob job) {
    queue.addLast(job);
    return queue.size();
  }

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
