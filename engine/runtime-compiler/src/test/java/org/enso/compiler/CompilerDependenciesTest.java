package org.enso.compiler;

import static org.junit.Assert.fail;

import org.junit.Ignore;
import org.junit.Test;

public final class CompilerDependenciesTest {
  @Ignore
  @Test
  public void noTruffleDependency() {
    assertNoClass("com.oracle.truffle.api.nodes.Node");
  }

  @Test
  public void noCirceDependency() {
    assertNoClass("io.circe.Error");
  }

  private static void assertNoClass(String name) {
    try {
      var c = Class.forName(name);
      var sb = new StringBuilder();
      sb.append("This class shouldn't be loaded: ").append(c.getName());
      sb.append("\nloaded from: ").append(c.getProtectionDomain().getCodeSource().getLocation());
      sb.append("\nclassloader: ").append(c.getClassLoader());
      sb.append("\nclasspath  : ").append(System.getProperty("java.class.path"));
      fail(sb.toString());
    } catch (ClassNotFoundException ex) {
      return;
    }
  }
}
