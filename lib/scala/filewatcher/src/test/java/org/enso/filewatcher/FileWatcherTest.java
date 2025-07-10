package org.enso.filewatcher;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.nio.file.ClosedWatchServiceException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.TimeUnit;
import org.enso.filewatcher.Watcher.WatcherEvent;
import org.enso.testkit.RetryTestRule;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

public class FileWatcherTest {
  private static final Logger LOGGER = LoggerFactory.getLogger(FileWatcherTest.class);
  private static final Level AT = Level.DEBUG;
  private static final long TIMEOUT_SECONDS = 5;

  @Rule public RetryTestRule retryRule = new RetryTestRule(3);
  @Rule public TemporaryFolder tmpFolder = new TemporaryFolder();

  private File tmpDir;
  private Path fileInTmpDir;
  private Path nestedDir;
  private Path fileInNestedDir;
  private ExecutorService executor;
  private BlockingQueue<WatcherEvent> eventQueue = new LinkedBlockingDeque<>();
  private Watcher watcher;
  private boolean isClosingWatcher;

  @Before
  public void before() throws IOException {
    isClosingWatcher = false;
    executor = Executors.newSingleThreadExecutor();
    tmpDir = tmpFolder.newFolder();
    fileInTmpDir = tmpDir.toPath().resolve("file.txt");
    Files.writeString(fileInTmpDir, "Initial content");
    nestedDir = tmpDir.toPath().resolve("nested-dir");
    Files.createDirectory(nestedDir);
    fileInNestedDir = nestedDir.resolve("nested-file.txt");
    Files.writeString(fileInNestedDir, "Initial content in nested file");
    eventQueue = new LinkedBlockingDeque<>();
    watcher =
        new DefaultWatcherFactory()
            .build(tmpDir.toPath(), this::eventCallback, this::exceptionCallback);
    watcher.start(executor);
  }

  @After
  public void after() throws Exception {
    assertThat(
        "No further events should be in the queue: " + eventQueue, eventQueue.isEmpty(), is(true));
    isClosingWatcher = true;
    eventQueue.clear();
    executor.shutdown();
    watcher.close();
  }

  private void eventCallback(WatcherEvent event) {
    try {
      eventQueue.put(event);
    } catch (InterruptedException e) {
      fail("Failed to put event in queue: " + e.getMessage());
    }
  }

  private void exceptionCallback(Watcher.WatcherError error) {
    // ClosedWatchServiceException is expected when closing the watcher
    if (!isClosingWatcher || !(error.throwable() instanceof ClosedWatchServiceException)) {
      var errMsg =
          String.format(
              "Unexpected Watcher error: %s '%s'",
              error.throwable(), error.throwable().getMessage());
      throw new AssertionError(errMsg);
    }
  }

  @Test
  public void tracksModificationToExistingFile() throws IOException {
    atomicAppend(fileInTmpDir, "Appended content");
    assertNextEventIs(modifyEvent(fileInTmpDir));
  }

  @Test
  public void tracksModificationToExistingNestedFile() throws IOException {
    atomicAppend(fileInNestedDir, "Appended content in nested file");
    assertNextEventIs(modifyEvent(fileInNestedDir));
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

  private void assertNextEventIs(WatcherEvent expectedEvent) {
    LOGGER.atLevel(AT).log("Expecting event: {}", expectedEvent);
    for (; ; ) {
      var event = pollEvent();
      LOGGER.atLevel(AT).log("  got event: {}" + event);
      if (expectedEvent.equals(event)) {
        LOGGER.atLevel(AT).log("  good!");
        return;
      }
    }
  }

  private WatcherEvent pollEvent() {
    try {
      return eventQueue.poll(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    } catch (InterruptedException e) {
      throw new AssertionError("Interrupted while waiting for event: " + e.getMessage(), e);
    }
  }

  private static WatcherEvent createEvent(Path path) {
    return new WatcherEvent(path, Watcher.EventType.CREATE);
  }

  private static WatcherEvent deleteEvent(Path path) {
    return new WatcherEvent(path, Watcher.EventType.DELETE);
  }

  private static WatcherEvent modifyEvent(Path path) {
    return new WatcherEvent(path, Watcher.EventType.MODIFY);
  }
}
