package org.enso.runner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

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
      m.mainEntry(line, Level.INFO, false);
    } catch (ExitCode ex) {
      assertEquals("Execution fails", 1, ex.exitCode);
      assertEquals("One line printed", 1, linesOut.size());
      assertEquals("Unrecognized option: --repl --inspect", linesOut.get(0));
      assertTrue("Also help was printed", m.helpPrinted);
    }
  }

  @Test
  public void cannotUseReplAndInspectAtOnce() throws Exception {
    try {
      var m = new MainMock();
      var file = tempDir.newFile("some.enso");
      var line = m.preprocessArguments("--repl", "--inspect", "--run", file.getAbsolutePath());
      m.mainEntry(line, Level.INFO, false);
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
    } catch (ExitCode e) {
      assertEquals("Execution fails", 1, e.exitCode);
      assertEquals("One line printed", 1, linesOut.size());
      assertThat(linesOut.get(0), containsString("must be in the form <property>=<value>"));
    }
  }

  private final class MainMock extends Main {
    boolean helpPrinted;

    @Override
    RuntimeException doExit(int exitCode) {
      throw new ExitCode(exitCode);
    }

    @Override
    void println(String msg) {
      linesOut.add(msg);
    }

    @Override
    void printHelp() {
      helpPrinted = true;
    }
  }

  private static final class ExitCode extends RuntimeException {
    final int exitCode;

    ExitCode(int exitCode) {
      this.exitCode = exitCode;
    }
  }
}
