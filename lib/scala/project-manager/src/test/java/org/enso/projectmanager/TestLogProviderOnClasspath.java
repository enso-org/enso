package org.enso.projectmanager;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;
import java.util.stream.Collectors;
import org.junit.Test;
import org.slf4j.spi.SLF4JServiceProvider;

/**
 * In the `runtime/Test` testing suite, {@link }org.enso.logger.TestLogProvider} should be among the
 * logging providers, because it is explicitly chosen as the logging provider for the tests.
 *
 * <p>Note that the same test is in the `runtime/Test` project.
 */
public class TestLogProviderOnClasspath {
  @Test
  public void testLogProviderIsOnClasspath() {
    var sl = ServiceLoader.load(SLF4JServiceProvider.class);
    var serviceIterator = sl.iterator();
    List<SLF4JServiceProvider> providers = new ArrayList<>();
    while (serviceIterator.hasNext()) {
      providers.add(serviceIterator.next());
    }
    List<String> providerNames =
        providers.stream().map(elem -> elem.getClass().getName()).collect(Collectors.toList());
    assertThat(providerNames, hasItem("org.enso.logger.TestLogProvider"));
  }

  @Test
  public void testLogProviderIsInUnnamedModule() {
    Class<?> testLogProviderClass = null;
    try {
      testLogProviderClass = Class.forName("org.enso.logger.TestLogProvider");
    } catch (ClassNotFoundException e) {
      fail("TestLogProvider class not found");
    }
    var mod = testLogProviderClass.getModule();
    assertThat(mod, notNullValue());
    assertThat("Should be an unnamed module - with null name", mod.getName(), nullValue());
  }
}
