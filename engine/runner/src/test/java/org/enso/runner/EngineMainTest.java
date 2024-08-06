package org.enso.runner;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import org.slf4j.event.Level;

public class EngineMainTest {
  private final List<String> linesOut = new ArrayList<>();

  @Test
  public void cannotUseReplAndInspectAtOnce() throws Exception {
    try {
      var m =
          new Main() {
            @Override
            RuntimeException doExit(int code) {
              throw new ExitCode(code);
            }

            void println(String line) {
              linesOut.add(line);
            }
          };
      var file = File.createTempFile("some", ".enso");
      file.deleteOnExit();
      var line = m.preprocessArguments("--repl", "--inspect", "--run", file.getAbsolutePath());
      m.mainEntry(line, Level.INFO, false);
    } catch (ExitCode ex) {
      assertEquals("Execution fails", 1, ex.exitCode);
      assertEquals("One line printed", 1, linesOut.size());
      assertEquals("Cannot use --inspect and --repl and --run at once", linesOut.get(0));
    }
  }

  private static final class ExitCode extends RuntimeException {
    final int exitCode;

    ExitCode(int exitCode) {
      this.exitCode = exitCode;
    }
  }
}
