package org.enso.runner;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;

interface ReplIO {
  /**
   * Ask user for a line of input, using given prompt
   *
   * @param prompt the prompt to display to the user
   * @return the user-provided input
   */
  UserInput readLine(String prompt);

  /**
   * Print a line to the REPL.
   *
   * @param contents contents of the line to print
   */
  void println(String contents);

  /**
   * Print a stack trace to the REPL.
   *
   * @param exception which stack trace is to be printed
   */
  default void printStackTrace(Exception exception) {
    var traceBuilder = new StringBuilder();
    var traceWriter =
        new Writer() {
          @Override
          public void write(char[] cbuf, int off, int len) {
            for (char c : cbuf) {
              traceBuilder.append(c);
            }
          }

          @Override
          public void flush() throws IOException {}

          @Override
          public void close() throws IOException {}
        };
    exception.printStackTrace(new PrintWriter(traceWriter));
    System.out.println(traceBuilder);
  }
}
