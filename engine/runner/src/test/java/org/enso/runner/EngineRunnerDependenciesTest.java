package org.enso.runner;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import org.apache.commons.cli.CommandLine;
import org.enso.runner.common.LanguageServerApi;
import org.enso.runner.common.ProfilingConfig;
import org.enso.runner.common.WrongOption;
import org.junit.Ignore;
import org.junit.Test;
import scala.Option;

public class EngineRunnerDependenciesTest {

  public EngineRunnerDependenciesTest() {}

  @Test
  public void unableToLoadClassFromLanguageServerProject() {
    try {
      var b = new CommandLine.Builder();
      b.addArg("server");
      var line = b.build();
      var prof = new ProfilingConfig(Option.empty(), Option.empty());
      LanguageServerApi.launchLanguageServer(line, prof, null);
      fail(
          "should generate a WrongOption error as support for --server option is missing on"
              + " classpath");
    } catch (WrongOption ex) {
      // OK
    }
  }

  @Test
  public void ableToLoadRClassFromEngineRunnerCommonProject() throws ClassNotFoundException {
    var c = Class.forName("org.enso.runner.common.LanguageServerApi");
    assertNotNull(
        "Should be able to load class from engine-runner-common project (obviously as we have"
            + " compile time dependency)",
        c);
  }
}
