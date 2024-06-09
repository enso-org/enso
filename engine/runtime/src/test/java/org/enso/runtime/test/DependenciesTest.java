package org.enso.runtime.test;

import static org.junit.Assert.fail;

import org.junit.Test;

public class DependenciesTest {
  @Test
  public void noAkkaDependencies() {
    try {
      Class.forName("akka.actor.ActorSystem$");
      fail("akka.actor.ActorSystem should not be on the classpath");
    } catch (ClassNotFoundException e) {
      // expected
    }
  }
}
