package org.enso.compiler.test.pass;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotSame;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.UUID;
import java.util.function.Function;
import org.enso.common.CachePreferences;
import org.enso.compiler.pass.analyse.alias.AliasMetadata;
import org.enso.compiler.pass.analyse.alias.graph.Graph;
import org.enso.compiler.pass.analyse.alias.graph.GraphBuilder;
import org.enso.compiler.pass.analyse.alias.graph.GraphOccurrence;
import org.enso.persist.Persistance;
import org.junit.Test;
import scala.Option;

public class PassPersistanceTest {
  private static final Persistance.Pool POOL =
      Persistance.Pool.merge(
          org.enso.compiler.core.ir.Persistables.POOL,
          org.enso.compiler.pass.analyse.Persistables.POOL,
          org.enso.compiler.pass.analyse.alias.graph.Persistables.POOL);

  @Test
  public void cachePreferences() throws Exception {
    var idSelf = UUID.randomUUID();
    var idBind = UUID.randomUUID();
    var pref = new CachePreferences(new HashMap<>());
    pref.set(idSelf, CachePreferences.Kind.SELF_ARGUMENT);
    pref.set(idBind, CachePreferences.Kind.BINDING_EXPRESSION);

    var out = serde(CachePreferences.class, pref, -1);
    assertEquals("Two elements", 2, out.preferences().size());
    assertEquals("They are structurally equal", pref, out);
    assertNotSame("But not ==", pref, out);
  }

  @Test
  public void graphScopePersistance() throws Exception {
    var b = GraphBuilder.create();
    var x = b.newDef("x", null, Option.empty());
    var b2 = b.addChild();
    var y = b2.newDef("y", null, Option.empty());
    var ux = b2.newUse("x", null, Option.empty(), true);
    var uy = b2.newUse("y", null, Option.empty(), true);

    assertGraphScopePersistance("Before serialization", b.toGraph(), y.id());

    var g = serde(Graph.class, b.toGraph(), -1);

    assertGraphScopePersistance("After deserialization", g, y.id());
  }

  private static void assertGraphScopePersistance(String prefix, Graph g, int yId) {
    var found = new ArrayList<GraphOccurrence.Def>();
    g.rootScope()
        .forEachOccurenceDefinition(
            (d) -> {
              found.add(d);
              return null;
            });

    assertEquals(prefix + ". One definition: " + found, 1, found.size());

    var childScope = g.scopeFor(yId).get();
    childScope.forEachOccurenceDefinition(
        (d) -> {
          found.add(d);
          return null;
        });
    assertEquals(prefix + ". One more definition", 2, found.size());
  }

  @Test
  public void aliasMetadataRootScope() throws Exception {
    var b = GraphBuilder.create();
    b.newDef("x", null, Option.empty());
    var graph = b.toGraph();

    var in = new AliasMetadata.RootScope(graph);
    var out = serde(AliasMetadata.RootScope.class, in, -1);

    assertEquals("They are structurally equal", in, out);
    assertNotSame("But not ==", in, out);
  }

  @Test
  public void aliasMetadataChildScope() throws Exception {
    var b = GraphBuilder.create();
    b.newDef("x", null, Option.empty());
    var child = b.addChild();
    child.newDef("y", null, Option.empty());
    var graph = b.toGraph();
    var childScope = child.toScope();

    var in = new AliasMetadata.ChildScope(graph, childScope);
    var out = serde(AliasMetadata.ChildScope.class, in, -1);
    System.out.print(in.scope().equals(out.scope()));

    assertEquals("They are structurally equal", in, out);
    assertNotSame("But not ==", in, out);
  }

  @Test
  public void aliasMetadataOccurrence() throws Exception {
    var b = GraphBuilder.create();
    var def = b.newDef("x", null, Option.empty());
    var graph = b.toGraph();

    var in = new AliasMetadata.Occurrence(graph, def.id());
    var out = serde(AliasMetadata.Occurrence.class, in, -1);

    assertEquals("They are structurally equal", in, out);
    assertNotSame("But not ==", in, out);
  }

  @Test
  public void aliasMetadataOccurrenceDifferentIds() throws Exception {
    var b = GraphBuilder.create();
    var defX = b.newDef("x", null, Option.empty());
    var defY = b.newDef("y", null, Option.empty());
    var graph = b.toGraph();

    var occ1 = new AliasMetadata.Occurrence(graph, defX.id());
    var occ2 = new AliasMetadata.Occurrence(graph, defY.id());

    assertNotEquals("Different occurrences are not equal", occ1, occ2);

    var out1 = serde(AliasMetadata.Occurrence.class, occ1, -1);
    var out2 = serde(AliasMetadata.Occurrence.class, occ2, -1);

    assertEquals("First roundtrips correctly", occ1, out1);
    assertEquals("Second roundtrips correctly", occ2, out2);
    assertNotEquals("Deserialized are still different", out1, out2);
  }

  @Test
  public void aliasMetadataRootAndChildScopeDiffer() throws Exception {
    var b = GraphBuilder.create();
    b.newDef("x", null, Option.empty());
    var child = b.addChild();
    child.newDef("y", null, Option.empty());
    var graph = b.toGraph();

    var root = new AliasMetadata.RootScope(graph);
    var childMeta = new AliasMetadata.ChildScope(graph, child.toScope());

    assertNotEquals("RootScope and ChildScope are not equal", root, childMeta);

    var rootOut = serde(AliasMetadata.RootScope.class, root, -1);
    var childOut = serde(AliasMetadata.ChildScope.class, childMeta, -1);

    assertNotEquals("Deserialized RootScope and ChildScope are not equal", rootOut, childOut);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize) throws IOException {
    return serde(clazz, l, expectedSize, null);
  }

  private static <T> T serde(Class<T> clazz, T l, int expectedSize, Function<Object, Object> fn)
      throws IOException {
    var arr = POOL.withWriteReplace(fn).write(l);
    if (expectedSize >= 0) {
      assertEquals(expectedSize, arr.length - 12);
    }
    var ref = POOL.read(arr);
    return ref.get(clazz);
  }
}
