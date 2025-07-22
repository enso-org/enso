package org.enso.runner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.slf4j.event.Level;

public class EngineMainTest {
  @Rule public TemporaryFolder tempDir = new TemporaryFolder();

  private final List<String> linesOut = new ArrayList<>();

  @Test
  public void unknownCommandBecauseTwoAreConcatenated() throws Exception {
    var m = new MainMock();
    try {
      var file = tempDir.newFile("some.enso");
      var line = m.preprocessArguments("--repl --inspect", "--run", file.getAbsolutePath());
      m.mainEntry(null, line, Level.INFO, false);
      fail("Expecting exception");
    } catch (ExitCode ex) {
      assertEquals("Execution fails", 1, ex.exitCode);
      assertEquals("One line printed", 1, linesOut.size());
      assertEquals("Unrecognized option: --repl --inspect", linesOut.get(0));
      assertTrue("Also help was printed", m.helpPrinted);
    }
  }

  /**
   * Following code used to yield an error.
   *
   * <pre>
   * java.lang.IllegalArgumentException: null
   *   at sun.nio.fs.UnixPath.subpath(UnixPath.java:338)
   *   at sun.nio.fs.UnixPath.subpath(UnixPath.java:52)
   *   at org.enso.runner.Utils.findFileAndProject(Utils.java:39)
   *   at org.enso.runner.Main.handleRun(Main.java:731)
   *   at org.enso.runner.Main.mainEntry(Main.java:1165)
   *   at org.enso.runner.EngineMainTest.nonExistingFile(EngineMainTest.java:57)
   * </pre>
   */
  @Test
  public void nonExistingFile() throws Exception {
    var m = new MainMock();
    var dir = tempDir.newFolder();
    var file = new File(dir, "non_existing.enso");
    try {
      var line = m.preprocessArguments("--run", file.getAbsolutePath());
      m.mainEntry(null, line, Level.INFO, false);
      fail("Expecting exception");
    } catch (ExitCode ex) {
      assertEquals("Execution fails", 1, ex.exitCode);
      assertEquals("No special output printed", 0, linesOut.size());
      assertFalse("No help was printed", m.helpPrinted);
      var out = ex.getMessage();
      assertEquals("No 'null' in the message: " + out, -1, out.indexOf("null"));
      assertEquals("File " + file + " does not exist.", out);
    }
  }

  @Test
  public void cannotUseReplAndInspectAtOnce() throws Exception {
    try {
      var m = new MainMock();
      var file = tempDir.newFile("some.enso");
      var line = m.preprocessArguments("--repl", "--inspect", "--run", file.getAbsolutePath());
      m.mainEntry(null, line, Level.INFO, false);
      fail("Expecting exception");
    } catch (ExitCode ex) {
      assertEquals("Execution fails", 1, ex.exitCode);
      assertEquals("One line printed", 1, linesOut.size());
      assertEquals("Cannot use --inspect and --repl and --run at once", linesOut.get(0));
    }
  }

  @Test
  public void canSetSystemProperty() throws IOException {
    var m = new MainMock();
    var file = tempDir.newFile("some.enso");
    var line = m.preprocessArguments("--run", file.getAbsolutePath(), "--vm.D", "foo=bar");
    var props = m.parseSystemProperties(line);
    assertEquals("bar", props.get("foo"));
  }

  @Test
  public void canSetMultipleSystemProperties() throws IOException {
    var m = new MainMock();
    var file = tempDir.newFile("some.enso");
    var line =
        m.preprocessArguments(
            "--run", file.getAbsolutePath(), "--vm.D", "foo=bar", "--vm.D", "baz=qux");
    var props = m.parseSystemProperties(line);
    assertEquals("bar", props.get("foo"));
    assertEquals("qux", props.get("baz"));
  }

  @Test
  public void systemPropertyHasDefaultValue() throws IOException {
    var m = new MainMock();
    var file = tempDir.newFile("some.enso");
    var line = m.preprocessArguments("--run", file.getAbsolutePath(), "--vm.D", "foo");
    var props = m.parseSystemProperties(line);
    assertEquals("true is the default value for property", "true", props.get("foo"));
  }

  @Test
  public void systemPropertyArgumentIncorrectFormat() throws IOException {
    try {
      var m = new MainMock();
      var file = tempDir.newFile("some.enso");
      var line = m.preprocessArguments("--run", file.getAbsolutePath(), "--vm.D", "foo=bar=baz");
      m.parseSystemProperties(line);
      fail("Expecting exception");
    } catch (ExitCode e) {
      assertEquals("Execution fails", 1, e.exitCode);
      assertEquals("One line printed", 1, linesOut.size());
      assertThat(linesOut.get(0), containsString("must be in the form <property>=<value>"));
    }
  }

  @Test
  public void genDocsFails_WhenProjectDoesNotExist() throws IOException {
    try {
      var m = new MainMock();
      // Using --docs api on purpose - as `--run` is able to detect project dir itself.
      var line =
          m.preprocessArguments("--in-project", "NON_EXISTING_DIR/foo/bar/xxx/zz", "--docs", "api");
      m.mainEntry(null, line, Level.INFO, false);
      fail("Expecting exception");
    } catch (ExitCode ex) {
      assertEquals("Execution fails", 1, ex.exitCode);
      assertEquals("No line printed", 0, linesOut.size());
      assertThat(ex.getMessage(), containsString("does not exist"));
    }
  }

  private final class MainMock extends Main {
    boolean helpPrinted;

    @Override
    RuntimeException doExit(int exitCode) {
      throw raise(RuntimeException.class, new ExitCode("MockExit", exitCode));
    }

    @Override
    void stdout(String msg) {
      linesOut.add(msg);
    }

    @Override
    void stderr(String msg) {
      linesOut.add(msg);
    }

    @Override
    void printHelp() {
      helpPrinted = true;
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
