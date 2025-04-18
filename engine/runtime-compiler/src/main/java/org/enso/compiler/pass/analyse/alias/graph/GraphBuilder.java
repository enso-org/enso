package org.enso.compiler.pass.analyse.alias.graph;

import java.util.Map;
import java.util.UUID;

/**
 * Builder of {@link Graph}. Separates the concerns of building a graph of local symbol definitions
 * and their usages from the actual querying of those symbols.
 */
public final class GraphBuilder {
  private final GraphBuilder parent;
  private final GraphImpl graph;
  private final ScopeImpl scope;
  private final Map<String, GraphOccurrence.Def> defs = new java.util.HashMap<>();

  private GraphBuilder() {
    var topLevel = Graph$.MODULE$.create();
    this.parent = null;
    this.graph = (GraphImpl) topLevel;
    this.scope = (ScopeImpl) topLevel.rootScope();
  }

  private GraphBuilder(GraphBuilder parent, Graph graph, Graph.Scope scope) {
    this.parent = parent;
    this.graph = (GraphImpl) graph;
    this.scope = (ScopeImpl) scope;
  }

  /**
   * Creates new empty builder.
   *
   * @return empty builder
   */
  public static GraphBuilder create() {
    return new GraphBuilder();
  }

  /**
   * Creates a builder for given graph and scope.
   *
   * @param g the graph
   * @param s its scope
   * @return builder operating on the graph {@code g} starting at scope {@code s}
   */
  public static GraphBuilder create(Graph g, Graph.Scope s) {
    assert g != null;
    assert s != null;
    var b = new GraphBuilder(null, g, s);
    // fillInDefinitions(b.scope, b.defs);
    return b;
  }

  /**
   * Creates a child scope and returns a builder for it.
   *
   * @return new builder for newly created scope, but the same graph
   */
  public GraphBuilder addChild() {
    return new GraphBuilder(this, graph, scope.addChild());
  }

  /**
   * Finds definition ID of provided symbol.
   *
   * @param name the name of the symbol
   * @return -1 if not such symbol found, otherwise ID of the symbol
   */
  public GraphOccurrence.Def findDef(String name) {
    return defs.get(name);
  }

  /** Creates new definition for */
  public GraphOccurrence.Def newDef(
      String symbol, java.util.UUID identifier, scala.Option<java.util.UUID> externalId) {
    return newDef(symbol, identifier, externalId, false, true);
  }

  public GraphOccurrence.Def newDef(
      String symbol,
      java.util.UUID identifier,
      scala.Option<java.util.UUID> externalId,
      boolean suspended,
      boolean addToScope) {
    var id = graph.nextId(addToScope ? scope : null);
    var slotIdx = addToScope ? scope.allDefinitions().size() : -1;
    var def =
        new GraphOccurrence.Def(id, slotIdx, symbol, identifier, externalId, suspended)
            .withScope(scope);
    if (addToScope) {
      scope.add(def);
      var prev = defs.put(symbol, def);
      // System.err.println(" defining " + symbol + " !");
      assert prev == null;
    }
    scope.addDefinition(def);
    return def;
  }

  /**
   * Factory method to create new [GraphOccurrence.Use].
   *
   * @param symbol symbol of the usage
   * @param identifier identifier or null
   * @param externalId external ID or null
   * @param resolve search for a {@link GraphOccurrence.Def} pair for this usage
   * @return
   */
  public GraphOccurrence.Use newUse(
      String symbol, UUID identifier, scala.Option<UUID> externalId, boolean resolve) {
    var use = GraphOccurrence.createUse(scope, graph.nextId(scope), symbol, identifier, externalId);
    if (resolve) {
      graph.resolveLocalUsage(use);
    }
    scope.add(use);
    return use;
  }

  /**
   * Freezes the associated graph from further modifications.
   *
   * @return this
   */
  public final GraphBuilder freeze() {
    graph.freeze();
    return this;
  }

  public Graph toGraph() {
    return graph;
  }

  public Graph.Scope toScope() {
    return scope;
  }

  private static void fillInDefinitions(ScopeImpl scope, Map<String, GraphOccurrence.Def> defs) {
    if (scope != null) {
      if (scope.parent().isDefined()) {
        fillInDefinitions(scope.parent().get(), defs);
      }
      scope.forEachOccurenceDefinition(
          o -> {
            if (o instanceof GraphOccurrence.Def d) {
              assert d.scope() == scope;
              defs.put(d.symbol(), d);
            }
            return null;
          });
    }
  }
}
