package org.enso.compiler.pass.analyse.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import org.enso.compiler.pass.analyse.alias.graph.GraphBuilder;
import org.enso.compiler.pass.analyse.alias.graph.GraphImpl;
import org.junit.Test;
import scala.Option;

public class GraphBuilderTest {
  @Test
  public void twoVariablesInOneScope() {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var y = root.newDef("y", null, Option.empty());

    var g = (GraphImpl) root.toGraph();
    assertEquals("One scope only", 1, g.numScopes());
    var s = (GraphImpl.Scope) g.rootScope();
    assertTrue("No child scopes", s.childScopes().isEmpty());
    assertEquals("Two variables", 2, s.allDefinitions().size());
  }

  @Test
  public void twoVariablesInTwoScope() {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var child = root.addChild();
    var y = child.newDef("y", null, Option.empty());

    var g = (GraphImpl) root.toGraph();
    assertEquals("Two scopes", 2, g.numScopes());
    var s = (GraphImpl.Scope) g.rootScope();
    assertEquals("One variable", 1, s.allDefinitions().size());
    assertEquals("One child scope", 1, s.childScopes().size());
    assertEquals("One variable in child", 1, s.childScopes().apply(0).allDefinitions().size());
  }
}
