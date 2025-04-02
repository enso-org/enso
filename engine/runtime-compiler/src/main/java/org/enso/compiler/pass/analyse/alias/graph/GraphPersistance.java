package org.enso.compiler.pass.analyse.alias.graph;

import java.io.IOException;
import org.enso.persist.Persistance;
import scala.Tuple2$;

public final class GraphPersistance {
  private GraphPersistance() {}

  @org.openide.util.lookup.ServiceProvider(service = Persistance.class)
  public static final class PersistAliasAnalysisGraphScope extends Persistance<GraphImpl.Scope> {
    public PersistAliasAnalysisGraphScope() {
      super(GraphImpl.Scope.class, false, 1267);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected GraphImpl.Scope readObject(Input in) throws IOException {
      var childScopes = in.readInline(scala.collection.immutable.List.class);
      var occurrencesValues = (scala.collection.immutable.Set<GraphOccurrence>) in.readObject();
      var occurrences = occurrencesValues.map(v -> Tuple2$.MODULE$.apply(v.id(), v)).toMap(null);
      var allDefinitions = in.readInline(scala.collection.immutable.List.class);
      var parent = new GraphImpl.Scope(childScopes, occurrences, allDefinitions);
      childScopes.forall(
          (object) -> {
            var ch = (GraphImpl.Scope) object;
            ch.withParent(parent);
            return null;
          });
      return parent;
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void writeObject(GraphImpl.Scope obj, Output out) throws IOException {
      out.writeInline(scala.collection.immutable.List.class, obj.childScopes());
      out.writeObject(obj.occurrences().values().toSet());
      out.writeInline(scala.collection.immutable.List.class, obj.allDefinitions());
    }
  }

  @org.openide.util.lookup.ServiceProvider(service = Persistance.class)
  public static final class PersistAliasAnalysisGraph extends Persistance<GraphImpl> {
    public PersistAliasAnalysisGraph() {
      super(GraphImpl.class, false, 1268);
    }

    @SuppressWarnings("unchecked")
    protected GraphImpl readObject(Input in) throws IOException {

      var rootScope = (GraphImpl.Scope) in.readObject();
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

    private static void assignParents(GraphImpl.Scope scope) {
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
