package org.enso.languageserver;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import org.junit.Test;

public class LanguageServerDependenciesTest {

  public LanguageServerDependenciesTest() {}

  @Test
  public void unableToLoadClassFromEngineRunnerProject() {
    try {
      var c = Class.forName("org.enso.runner.Main");
      fail("Shouldn't be able to load class from engine-runner project " + c);
    } catch (ClassNotFoundException ex) {
      // OK
    }
  }

  @Test
  public void ableToLoadClassFromEngineRunnerCommonProject() throws ClassNotFoundException {
    var c = Class.forName("org.enso.runner.common.LanguageServerApi");
    assertNotNull(
        "Should be able to load class from engine-runner-common project (obviously as we have"
            + " compile time dependency)",
        c);
  }
}
