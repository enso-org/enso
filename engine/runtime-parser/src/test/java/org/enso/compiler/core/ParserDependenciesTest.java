package org.enso.compiler.core;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

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

  @Test
  public void parserProcessorIsAvailable() {
    try {
      var clazz = Class.forName("org.enso.runtime.parser.processor.IRProcessor");
      assertThat(clazz, is(notNullValue()));
    } catch (ClassNotFoundException e) {
      fail(e.getMessage());
    }
  }
}
