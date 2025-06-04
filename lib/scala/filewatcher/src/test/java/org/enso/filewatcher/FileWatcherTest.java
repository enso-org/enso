package org.enso.filewatcher;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;
import org.enso.filewatcher.Watcher.EventTypeCreate$;
import org.enso.filewatcher.Watcher.WatcherEvent;
import org.enso.testkit.RetryTestRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import scala.runtime.BoxedUnit;

public class FileWatcherTest {
  private static final long TIMEOUT_SECONDS = 5;

  @Rule public RetryTestRule retryRule = new RetryTestRule(3);
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  private File tmpDir;
  private ExecutorService executor;
  private BlockingQueue<Watcher.WatcherEvent> eventQueue = new LinkedBlockingDeque<>();
  private Watcher watcher;

  @Before
  public void before() throws IOException {
    executor = Executors.newSingleThreadExecutor();
    tmpDir = tmpFolder.newFolder();
    eventQueue = new LinkedBlockingDeque<>();
    watcher =
        new JDKWatcherFactory()
            .build(tmpDir.toPath(), this::eventCallback, this::exceptionCallback);
    watcher.start(executor);
  }

  @After
  public void after() {
    eventQueue.clear();
    executor.shutdown();
    watcher.stop();
  }

  private BoxedUnit eventCallback(Watcher.WatcherEvent event) {
    try {
      eventQueue.put(event);
    } catch (InterruptedException e) {
      fail("Failed to put event in queue: " + e.getMessage());
    }
    return null;
  }

  private BoxedUnit exceptionCallback(Watcher.WatcherError error) {
    throw new AssertionError("Watcher error: " + error.throwable().getMessage(), error.throwable());
  }

  @Test
  public void receiveCreateEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");
    Files.createFile(fileA);
    WatcherEvent event = pollEvent();
    assertThat(event.eventType(), is(createEventType()));
    var expectedPath = tmpDir.toPath().resolve("a.txt");
    assertThat(event.path(), is(expectedPath));
  }

  private WatcherEvent pollEvent() {
    try {
      return eventQueue.poll(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      throw new AssertionError("Interrupted while waiting for event: " + e.getMessage(), e);
    }
  }

  private static Watcher.EventType createEventType() {
    return EventTypeCreate$.MODULE$;
  }
}
