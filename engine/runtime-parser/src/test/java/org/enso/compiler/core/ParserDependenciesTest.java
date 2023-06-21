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
}
