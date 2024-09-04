package org.enso.interpreter.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.lang.invoke.MethodHandles;
import org.junit.Test;

/**
 * Test source in `runtime-integration-tests` sbt project should be patched to the
 * `org.enso.runtime` module. This is necessary to allow calling Java methods from Enso code.
 * Moreover, there are other testing facilities that require reflective access to the classes inside
 * `org.enso.runtime` module. One of the facilities is {@link
 * com.oracle.truffle.host.HostClassDesc#getLookup(Class, HostClassCache) truffle host lookup}.
 */
public class TestJPMSModuleConfiguration {

  @Test
  public void testPackagesArePatchedToRuntimeModule() {
    var clazz = VectorTest.class;
    var modName = clazz.getModule().getName();
    assertThat(
        "Test packages are patched to org.enso.runtime module", modName, is("org.enso.runtime"));
  }

  @Test
  public void testClassIsOpenForLookup() throws IllegalAccessException {
    // org.enso.interpreter.test.VectorTest contains public static methods that
    // are called from Enso test code.
    var clazz = VectorTest.class;
    assertThat(
        "Test package is exported unconditionally",
        clazz.getModule().isExported("org.enso.interpreter.test"),
        is(true));
    assertThat(
        "Package is opened unconditionally",
        clazz.getModule().isOpen("org.enso.interpreter.test"),
        is(true));
    var lookup = MethodHandles.publicLookup();
    var newClazz = lookup.accessClass(clazz);
    assertThat(newClazz, is(notNullValue()));
  }
}
