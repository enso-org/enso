package org.enso.compiler.core;

import static org.junit.Assert.assertNull;

import org.junit.Test;

public class ParserDependenciesTest {

  public ParserDependenciesTest() {}

  @Test
  public void avoidCatsDependency() {
    try {
      var clazz = Class.forName("cats.Align");
      assertNull("No cats classes shall be available in the parser project", clazz);
    } catch (ClassNotFoundException ex) {
      // correct
    }
  }

  @Test
  public void avoidTruffleDependency() {
    try {
      var clazz = Class.forName("com.oracle.truffle.api.source.Source");
      assertNull("No Truffle classes shall be available in the parser project", clazz);
    } catch (ClassNotFoundException ex) {
      // correct
    }
  }

  @Test
  public void avoidPolyglotDependency() {
    try {
      var clazz = Class.forName("org.graalvm.polyglot.Source");
      assertNull("No GraalVM polyglot classes shall be available in the parser project", clazz);
    } catch (ClassNotFoundException ex) {
      // correct
    }
  }
}
