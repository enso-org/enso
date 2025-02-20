package org.enso.compiler.pass.analyse.alias.graph;

/**
 * Builder of {@link Graph}. Separates the concerns of building a graph of local symbol definitions
 * and their usages from the actual querying of those symbols.
 */
public final class GraphBuilder {
  private final Graph graph;
  private final Graph.Scope scope;

  private GraphBuilder(Graph graph, Graph.Scope scope) {
    this.graph = graph;
    this.scope = scope;
  }

  /**
   * Creates new empty builder.
   *
   * @return empty builder
   */
  public static GraphBuilder create() {
    var topLevel = Graph$.MODULE$.create();
    return create(topLevel, topLevel.rootScope());
  }

  /**
   * Creates a builder for given graph and scope.
   *
   * @param g the graph
   * @param s its scope
   * @return builder operating on the graph {@code g} starting at scope {@code s}
   */
  public static GraphBuilder create(Graph g, Graph.Scope s) {
    return new GraphBuilder(g, s);
  }

  /**
   * Creates a child scope and returns a builder for it.
   *
   * @return new builder for newly created scope, but the same graph
   */
  public GraphBuilder addChild() {
    return new GraphBuilder(graph, scope.addChild());
  }

  /**
   * Adds occurrence to current scope.
   *
   * @return this builder with modified scope
   */
  public GraphBuilder add(GraphOccurrence occ) {
    this.scope.add(occ);
    return this;
  }

  /**
   * Adds definition to current scope.
   *
   * @return this builder with modified scope
   */
  public GraphBuilder addDefinition(GraphOccurrence.Def def) {
    this.scope.addDefinition(def);
    return this;
  }

  /**
   * Finds definition ID of provided symbol.
   *
   * @param name the name of the symbol
   * @return -1 if not such symbol found, otherwise ID of the symbol
   */
  public int findDef(String name) {
    var first = this.scope.occurrences().values().find(occ -> occ.symbol().equals(name));
    if (first.nonEmpty() && first.get() instanceof GraphOccurrence.Def def) {
      return def.id();
    } else {
      return -1;
    }
  }

  /** Creates new definition for */
  public GraphOccurrence.Def newDef(
      String symbol, java.util.UUID identifier, scala.Option<java.util.UUID> externalId) {
    return newDef(symbol, identifier, externalId, false);
  }

  public GraphOccurrence.Def newDef(
      String symbol,
      java.util.UUID identifier,
      scala.Option<java.util.UUID> externalId,
      boolean suspended) {
    return new GraphOccurrence.Def(graph.nextId(), symbol, identifier, externalId, suspended);
  }

  /** Factory method to create new [GraphOccurrence.Use]. */
  public GraphOccurrence.Use newUse(
      String symbol, java.util.UUID identifier, scala.Option<java.util.UUID> externalId) {
    return new GraphOccurrence.Use(graph.nextId(), symbol, identifier, externalId);
  }

  public void resolveLocalUsage(GraphOccurrence.Use use) {
    graph.resolveLocalUsage(use);
  }

  public Graph toGraph() {
    return graph;
  }

  public Graph.Scope toScope() {
    return scope;
  }
}
