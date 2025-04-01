package org.enso.compiler.pass.analyse.test;

import static org.junit.Assert.*;

import org.enso.compiler.pass.analyse.alias.graph.GraphBuilder;
import org.junit.Test;
import scala.Option;

public class GraphBuilderTest {
  @Test
  public void twoVariablesInOneScope() {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var y = root.newDef("y", null, Option.empty());

    var g = root.toGraph();
    assertEquals("One scope only", 1, g.numScopes());
    var s = g.rootScope();
    assertTrue("No child scopes", s.childScopes().isEmpty());
    assertEquals("Two variables", 2, s.allDefinitions().size());
  }

  @Test
  public void twoVariablesInTwoScope() {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var child = root.addChild();
    var y = child.newDef("y", null, Option.empty());

    var g = root.toGraph();
    assertEquals("Two scopes", 2, g.numScopes());
    var s = g.rootScope();
    assertEquals("One variable", 1, s.allDefinitions().size());
    assertEquals("One child scope", 1, s.childScopes().size());
    assertEquals("One variable in child", 1, s.childScopes().apply(0).allDefinitions().size());
  }
}
