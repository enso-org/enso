package org.enso.filewatcher;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;
import org.enso.filewatcher.JWatcherEvent.EventType;
import org.enso.testkit.RetryTestRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class FileWatcherTest {
  private static final long TIMEOUT_SECONDS = 5;

  @Rule public RetryTestRule retryRule = new RetryTestRule(3);
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  private File tmpDir;
  private ExecutorService executor;
  private BlockingQueue<JWatcherEvent> eventQueue = new LinkedBlockingDeque<>();
  private JWatcher watcher;

  @Before
  public void before() throws IOException {
    executor = Executors.newSingleThreadExecutor();
    tmpDir = tmpFolder.newFolder();
    eventQueue = new LinkedBlockingDeque<>();
    watcher = JWatcher.create(tmpDir.toPath(), this::eventCallback, this::exceptionCallback);
    watcher.start(executor);
  }

  @After
  public void after() {
    eventQueue.clear();
    executor.shutdown();
    watcher.stop();
  }

  private void eventCallback(JWatcherEvent event) {
    try {
      eventQueue.put(event);
    } catch (InterruptedException e) {
      fail("Failed to put event in queue: " + e.getMessage());
    }
  }

  private void exceptionCallback(JWatcher.JWatcherError error) {
    throw new AssertionError("Watcher error: " + error.throwable().getMessage(), error.throwable());
  }

  @Test
  public void receiveCreateEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");
    Files.createFile(fileA);
    var event = pollEvent();
    var expectedPath = tmpDir.toPath().resolve("a.txt");
    assertThat(event, is(new JWatcherEvent(expectedPath, EventType.CREATE)));
  }

  @Test
  public void receiveDeleteEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");

    Files.createFile(fileA);
    var event1 = pollEvent();
    assertThat(event1, is(createEvent(fileA)));

    Files.delete(fileA);
    var event2 = pollEvent();
    assertThat(event2, is(deleteEvent(fileA)));
  }

  @Test
  public void receiveModifyEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");

    Files.createFile(fileA);
    var event1 = pollEvent();
    assertThat(event1, is(createEvent(fileA)));

    Files.writeString(fileA, "Hello, World!");
    var event2 = pollEvent();
    assertThat(event2, is(modifyEvent(fileA)));
  }

  @Test
  public void receiveEventsFromSubdirectories() throws IOException {
    var subdir = Paths.get(tmpDir.getPath(), "subdir");
    var fileA = Paths.get(tmpDir.getPath(), "subdir", "a.txt");
    Files.createDirectories(subdir);
    var event1 = pollEvent();
    assertThat(event1, is(createEvent(subdir)));

    Files.createFile(fileA);
    var event2 = pollEvent();
    assertThat(event2, is(createEvent(fileA)));
  }

  private JWatcherEvent pollEvent() {
    try {
      return eventQueue.poll(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      throw new AssertionError("Interrupted while waiting for event: " + e.getMessage(), e);
    }
  }

  private static JWatcherEvent createEvent(Path path) {
    return new JWatcherEvent(path, EventType.CREATE);
  }

  private static JWatcherEvent deleteEvent(Path path) {
    return new JWatcherEvent(path, EventType.DELETE);
  }

  private static JWatcherEvent modifyEvent(Path path) {
    return new JWatcherEvent(path, EventType.MODIFY);
  }
}
