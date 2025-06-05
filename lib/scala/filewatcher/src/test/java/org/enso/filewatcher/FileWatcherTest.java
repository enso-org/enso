package org.enso.filewatcher;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
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
    assertThat(
        "No further events should be in the queue: " + eventQueue, eventQueue.isEmpty(), is(true));
    eventQueue.clear();
    executor.shutdown();
    watcher.close();
  }

  private void eventCallback(JWatcherEvent event) {
    try {
      eventQueue.put(event);
    } catch (InterruptedException e) {
      fail("Failed to put event in queue: " + e.getMessage());
    }
  }

  private void exceptionCallback(JWatcher.JWatcherError error) {
    throw new AssertionError(
        "Unexpected Watcher error: " + error.throwable().getMessage(), error.throwable());
  }

  @Test
  public void receiveCreateEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");
    Files.createFile(fileA);
    assertNextEventIs(createEvent(fileA));
  }

  @Test
  public void receiveDeleteEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");

    Files.createFile(fileA);
    assertNextEventIs(createEvent(fileA));

    Files.delete(fileA);
    assertNextEventIs(deleteEvent(fileA));
  }

  @Test
  public void receiveModifyEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");

    Files.createFile(fileA);
    assertNextEventIs(createEvent(fileA));

    atomicAppend(fileA, "Hello, World!");
    assertNextEventIs(modifyEvent(fileA));
  }

  @Test
  public void receiveMultipleModifyEvents() throws IOException {
    var fileA = Paths.get(tmpDir.getPath(), "a.txt");

    Files.createFile(fileA);
    assertNextEventIs(createEvent(fileA));

    atomicAppend(fileA, "Hello, World!");
    assertNextEventIs(modifyEvent(fileA));

    atomicAppend(fileA, "Nazdar!");
    assertNextEventIs(modifyEvent(fileA));
  }

  @Test
  public void receiveEventsFromSubdirectories() throws IOException {
    var subdir = Paths.get(tmpDir.getPath(), "subdir");
    var fileA = Paths.get(tmpDir.getPath(), "subdir", "a.txt");
    Files.createDirectories(subdir);
    assertNextEventIs(createEvent(subdir));

    Files.createFile(fileA);
    assertNextEventIs(createEvent(fileA));
  }

  @Test
  public void receiveModifyEventInSubdir() throws IOException {
    var subdir = Paths.get(tmpDir.getPath(), "subdir");
    var fileA = Paths.get(tmpDir.getPath(), "subdir", "a.txt");
    Files.createDirectories(subdir);
    assertNextEventIs(createEvent(subdir));

    Files.createFile(fileA);
    assertNextEventIs(createEvent(fileA));

    atomicAppend(fileA, "Hello, World!");
    assertNextEventIs(modifyEvent(fileA));
  }

  @Test
  public void receiveEventsFromNestedSubdirectories() throws IOException {
    var dir = Paths.get(tmpDir.getPath(), "dir");
    var subdir = Paths.get(tmpDir.getPath(), "dir", "subdir");
    var file = Paths.get(tmpDir.getPath(), "dir", "subdir", "a.txt");
    Files.createDirectories(dir);
    assertNextEventIs(createEvent(dir));
    Files.createDirectories(subdir);
    assertNextEventIs(createEvent(subdir));
    Files.createFile(file);
    assertNextEventIs(createEvent(file));
  }

  @Test
  public void receiveEvents_AfterSubdirWasDeletedAndRecreated() throws IOException {
    var subdir = Paths.get(tmpDir.getPath(), "subdir");
    var fileA = Paths.get(tmpDir.getPath(), "subdir", "a.txt");
    Files.createDirectories(subdir);
    assertNextEventIs(createEvent(subdir));

    Files.delete(subdir);
    assertNextEventIs(deleteEvent(subdir));

    Files.createDirectories(subdir);
    assertNextEventIs(createEvent(subdir));

    Files.createFile(fileA);
    assertNextEventIs(createEvent(fileA));
  }

  /**
   * Atomically (with respect to other FS operations) appends string to the file.
   *
   * @param path Must already be a file and exist.
   */
  private static void atomicAppend(Path path, String content) throws IOException {
    Files.writeString(path, content, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
  }

  private void assertNextEventIs(JWatcherEvent expectedEvent) {
    var event = pollEvent();
    assertThat(event, is(expectedEvent));
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
