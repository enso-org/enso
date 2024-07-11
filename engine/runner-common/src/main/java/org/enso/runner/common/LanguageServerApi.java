package org.enso.runner.common;

import java.util.ServiceLoader;
import org.apache.commons.cli.CommandLine;
import org.slf4j.event.Level;

public abstract class LanguageServerApi {
  public static void launchLanguageServer(CommandLine line, Level logLevel) {
    var it = ServiceLoader.load(LanguageServerApi.class).iterator();
    var impl = it.next();
    impl.runLanguageServer(line, logLevel);
  }

  protected abstract void runLanguageServer(CommandLine line, Level logLevel);
}
