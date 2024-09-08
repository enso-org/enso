package org.enso.runner;

import java.io.IOException;
import java.nio.file.Path;
import org.jline.reader.EndOfFileException;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.UserInterruptException;
import org.jline.reader.impl.DefaultParser;
import org.jline.reader.impl.history.DefaultHistory;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;

final class TerminalIO implements ReplIO {
  private final Terminal terminal;
  private final LineReader lineReader;

  TerminalIO(Path historyFilePath) {
    // jline uses the class loader from `Thread.currentThread().getContextClassLoader()` to
    // load services. We need to override the context class loader to be `IsolatedClassLoader`
    // from the runner.jar class loader.
    var prevClassLoader = Thread.currentThread().getContextClassLoader();
    Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
    try {
      terminal = TerminalBuilder.builder().system(true).build();
    } catch (IOException e) {
      throw new IllegalStateException(e);
    }
    var parser = new DefaultParser();
    parser.setEscapeChars(null);
    var history = new DefaultHistory();
    lineReader =
        LineReaderBuilder.builder()
            .parser(parser)
            .variable(LineReader.HISTORY_FILE, historyFilePath)
            .history(history)
            .terminal(terminal)
            .build();
    Thread.currentThread().setContextClassLoader(prevClassLoader);

    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  try {
                    history.save();
                  } catch (IOException e) {
                    System.err.println("Failed to save REPL history: " + e);
                  }
                }));
  }

  @Override
  public UserInput readLine(String prompt) {
    try {
      return new Line(lineReader.readLine(prompt));
    } catch (UserInterruptException | EndOfFileException e) {
      return new EndOfInput();
    }
  }

  @Override
  public void println(String contents) {
    terminal.writer().println(contents);
  }
}
