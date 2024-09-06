package org.enso.runner;

import org.enso.polyglot.debugger.ReplExecutor;
import org.enso.polyglot.debugger.SessionManager;
import scala.runtime.Nothing$;

final class Repl implements SessionManager {
  private final ReplIO replIO;

  Repl(ReplIO replIO) {
    this.replIO = replIO;
  }

  @Override
  public Nothing$ startSession(ReplExecutor executor) {
    var continueRunning = true;
    while (continueRunning) {
      var input = replIO.readLine("> ");
      switch (input) {
        case EndOfInput eoi -> continueRunning = false;
        case Line line when !line.getLine().isEmpty() -> {
          var content = line.getLine();
          switch (content) {
            case ":list", ":l" -> {
              var bindings = executor.listBindings();
              bindings.foreachEntry((name, value) -> {
                replIO.println(name + " = " + value);
                return null;
              });
            }
            case ":quit", "q" -> {
              continueRunning = false;
            }
            default -> {
              var result = executor.evaluate(content);
              if (result.isLeft()) {
                @SuppressWarnings("deprecation")
                var ex = result.left().get();
                replIO.println("Evaluation failed with: " + ex.getMessage());
                replIO.printStackTrace(ex);
              } else {
                @SuppressWarnings("deprecation")
                var objectRepr = result.right().get();
                replIO.println(">>> " + objectRepr);
              }
            }
          }
        }
        default -> {
          // nop
        }
      }
    }

    return (Nothing$) executor.exit();
  }
}
