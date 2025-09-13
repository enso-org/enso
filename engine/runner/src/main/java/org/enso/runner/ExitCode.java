package org.enso.runner;

import java.io.IOException;

/**
 * Thrown to instruct the {@link Main} launcher to exit with given exit code and provided error
 * message.
 */
final class ExitCode extends IOException {
  final int exitCode;

  ExitCode(String msg, int exitCode) {
    super(msg);
    assert msg != null;
    this.exitCode = exitCode;
  }
}
