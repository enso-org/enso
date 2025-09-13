package org.enso.compiler.pass.analyse.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.enso.compiler.pass.analyse.alias.graph.GraphBuilder;
import org.enso.compiler.pass.analyse.alias.graph.GraphImpl;
import org.enso.compiler.pass.analyse.alias.graph.ScopeImpl;
import org.junit.Ignore;
import org.junit.Test;
import scala.Option;
import scala.collection.mutable.HashMap;

public class GraphBuilderTest {
  @Test
  public void twoVariablesInOneScope() {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var y = root.newDef("y", null, Option.empty());

    var g = (GraphImpl) root.toGraph();
    assertEquals("One scope only", 1, g.numScopes());
    var s = (ScopeImpl) g.rootScope();
    assertTrue("No child scopes", s.childScopes().isEmpty());
    assertEquals("Two variables", 2, s.allDefinitions().size());
  }

  @Test
  public void twoVariablesInTwoScopes() {
    doTwoVariablesInTwoScopes(false);
  }

  @Test
  public void twoVariablesInTwoScopedWithCopy() {
    doTwoVariablesInTwoScopes(true);
  }

  private void doTwoVariablesInTwoScopes(boolean deepCopy) {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var child = root.addChild();
    var y = child.newDef("y", null, Option.empty());

    var g = (GraphImpl) root.toGraph();
    var map = new HashMap<GraphImpl.Scope, GraphImpl.Scope>();

    if (deepCopy) {
      g = (GraphImpl) g.deepCopy(map);
    }

    assertEquals("Two scopes", 2, g.numScopes());
    var s = (ScopeImpl) g.rootScope();
    assertEquals("One variable", 1, s.allDefinitions().size());
    assertEquals("One child scope", 1, s.childScopes().size());
    var chScope = s.childScopes().apply(0);
    assertEquals("One variable in child", 1, chScope.allDefinitions().size());
    var def = chScope.allDefinitions().iterator().next();
    assertSame(chScope, def.scope());
  }

  @Test
  @Ignore
  public void freshGraphFromMultipleScopes() {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var child = root.addChild();
    var y = child.newDef("y", null, Option.empty());

    var fresh = GraphBuilder.create(child.toGraph(), child.toScope());

    assertEquals("found in child scope", y, fresh.findDef("y"));
    assertEquals("found in parent scope", x, fresh.findDef("x"));
  }

  @Test
  public void indexInAScope() {
    var root = GraphBuilder.create();
    var x = root.newDef("x", null, Option.empty());
    var child = root.addChild();
    var y = child.newDef("y", null, Option.empty());
    var z = root.newDef("z", null, Option.empty());
    assertEquals("Zero index", 0, x.slotIndx());
    assertEquals("One index", 1, z.slotIndx());
    assertEquals("Zero index again", 0, y.slotIndx());
  }
}
