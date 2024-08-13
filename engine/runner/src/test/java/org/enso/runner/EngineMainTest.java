package org.enso.runner;

import static org.junit.Assert.assertEquals;

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

  private final class MainMock extends Main {
    @Override
    RuntimeException doExit(int exitCode) {
      throw new ExitCode(exitCode);
    }

    @Override
    void println(String msg) {
      linesOut.add(msg);
    }
  }

  private static final class ExitCode extends RuntimeException {
    final int exitCode;

    ExitCode(int exitCode) {
      this.exitCode = exitCode;
    }
  }
}
