package org.enso.compiler.pass.analyse.alias.graph;

import java.io.IOException;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import scala.collection.immutable.HashMap;

public final class GraphPersistance {
  private GraphPersistance() {}

  @Persistable(id = 1267)
  public static final class PersistAliasAnalysisGraphScope extends Persistance<ScopeImpl> {
    public PersistAliasAnalysisGraphScope() {
      super(ScopeImpl.class, false, 1267);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected ScopeImpl readObject(Input in) throws IOException {
      var childScopes = in.readInline(scala.collection.immutable.List.class);
      var occurrencesValues = (scala.collection.immutable.Set<GraphOccurrence>) in.readObject();
      var allDefinitions = in.readInline(java.util.List.class);
      var parent = new ScopeImpl(childScopes, new HashMap<>(), allDefinitions);
      occurrencesValues.foreach(
          v -> {
            var associated = v.withScope(parent);
            assert associated.scope() == parent;
            return null;
          });

      childScopes.forall(
          (object) -> {
            var ch = (ScopeImpl) object;
            ch.withParent(parent);
            return null;
          });
      return parent;
    }

    @Override
    protected void writeObject(ScopeImpl obj, Output out) throws IOException {
      obj.writeObject(out);
    }
  }

  @Persistable(id = 1268)
  public static final class PersistAliasAnalysisGraph extends Persistance<GraphImpl> {
    public PersistAliasAnalysisGraph() {
      super(GraphImpl.class, false, 1268);
    }

    @SuppressWarnings("unchecked")
    protected GraphImpl readObject(Input in) throws IOException {

      var rootScope = (ScopeImpl) in.readObject();
      assignParents(rootScope);

      var links =
          (scala.collection.immutable.Set) in.readInline(scala.collection.immutable.Set.class);

      var nextIdCounter = in.readInt();
      var g = new GraphImpl(rootScope, nextIdCounter, links);
      return g;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void writeObject(GraphImpl obj, Output out) throws IOException {
      out.writeObject(obj.rootScope());
      out.writeInline(scala.collection.immutable.Set.class, obj.getLinks());
      out.writeInt(obj.nextIdCounter());
    }

    private static void assignParents(ScopeImpl scope) {
      scope
          .childScopes()
          .foreach(
              (ch) -> {
                assignParents(ch);
                ch.withParent(scope);
                return null;
              });
    }
  }
}
