package org.enso.runtime.parser.processor.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.contains;
import static org.junit.Assert.fail;

import java.util.Map;
import java.util.Set;
import org.enso.runtime.parser.processor.utils.DependencySorter;
import org.enso.runtime.parser.processor.utils.DependencySorter.CyclicDependencyException;
import org.junit.Test;

public class TestDependencySorter {
  @Test
  public void noCycle() {
    var deps = Map.of("A", Set.of("B"));
    try {
      DependencySorter.ensureNoCycles(deps);
    } catch (CyclicDependencyException e) {
      fail("Unexpected cyclic dependency: " + e.getMessage());
    }
  }

  @Test
  public void simpleCycle() {
    var deps = Map.of("A", Set.of("B", "A"));
    try {
      DependencySorter.ensureNoCycles(deps);
      fail("Expected cyclic dependency");
    } catch (CyclicDependencyException e) {
      // expected
    }
  }

  @Test
  public void simpleCycle_2() {
    var deps =
        Map.of(
            "A", Set.of("B"),
            "B", Set.of("A"));
    try {
      DependencySorter.ensureNoCycles(deps);
      fail("Expected cyclic dependency");
    } catch (CyclicDependencyException e) {
      // expected
    }
  }

  @Test
  public void topologicalSort_1() {
    var deps = Map.of("A", Set.of("B"));
    var sorted = DependencySorter.topologicalSort(deps);
    assertThat(sorted, contains("B", "A"));
  }

  @Test
  public void topologicalSort_2() {
    var deps =
        Map.of(
            "A", Set.of("B"),
            "B", Set.of("C", "D"));
    var sorted = DependencySorter.topologicalSort(deps);
    assertThat(sorted, anyOf(contains("D", "C", "B", "A"), contains("C", "D", "B", "A")));
  }
}
