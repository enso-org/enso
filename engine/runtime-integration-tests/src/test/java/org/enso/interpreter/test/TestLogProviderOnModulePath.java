package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.enso.logging.service.logback.test.provider.TestLogProvider;
import org.junit.Test;

/**
 * In the `runtime/Test` testing suite, {@link TestLogProvider} should be among the logging
 * providers, because it is explicitly chosen as the logging provider for the tests.
 */
public class TestLogProviderOnModulePath {
  @Test
  public void testLogProviderOnClasspath() {
    var modName = "org.enso.logging.service.logback.test.provider";
    var logProviderMod = ModuleLayer.boot().findModule(modName);
    assertThat(logProviderMod.isPresent(), is(true));
  }
}
