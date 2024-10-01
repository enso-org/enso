package org.enso.compiler.test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.enso.compiler.MetadataInteropHelpers;
import org.junit.Test;

public final class CompilerDependenciesTest {
  @Test
  public void compilerModuleIsLoaded() {
    var compilerModule = MetadataInteropHelpers.class.getModule();
    assertTrue(
        "org.enso.runtime.compiler module is loaded and the test classes are "
            + "patched into the module",
        compilerModule.isNamed());
  }

  @Test
  public void noTruffleDependency() {
    assertNoClassInCompilerModule("com.oracle.truffle.api.nodes.Node");
  }

  @Test
  public void noJacksonDependency() {
    assertNoClassInCompilerModule("com.fasterxml.jackson.databind.ObjectMapper");
  }

  private static void assertNoClassInCompilerModule(String name) {
    var compilerModule = MetadataInteropHelpers.class.getModule();
    var classLoader = compilerModule.getClassLoader();
    try {
      var c = classLoader.loadClass(name);
      var sb = new StringBuilder();
      sb.append("This class shouldn't be loaded: ").append(c.getName());
      sb.append("\nloaded from: ").append(c.getProtectionDomain().getCodeSource().getLocation());
      sb.append("\nclassloader: ").append(c.getClassLoader());
      sb.append("\nmoduleLayer  : ").append(compilerModule.getLayer());
      fail(sb.toString());
    } catch (ClassNotFoundException ex) {
      return;
    }
  }
}
