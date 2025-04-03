package org.enso.compiler.pass.analyse.alias;

import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.analyse.AliasAnalysis;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.alias.graph.Graph;
import org.enso.compiler.pass.analyse.alias.graph.GraphBuilder;
import scala.Option;

/** Metata for {@link AliasAnalysis} pass. */
@SuppressWarnings("unchecked")
public abstract sealed class AliasMetadata implements IRPass.IRMetadata
    permits AliasMetadata.Scope, AliasMetadata.Occurrence {
  private final Graph graph;

  private AliasMetadata(Graph g) {
    this.graph = g;
  }

  public static <I extends IR> I updateMetadata(I ir, AliasMetadata data) {
    ir.passData().update(AliasAnalysis$.MODULE$, data);
    return ir;
  }

  public final Graph graph() {
    return graph;
  }

  @Override
  public final ProcessingPass.Metadata prepareForSerialization(CompilerStub compiler) {
    return this;
  }

  @Override
  public final Option<ProcessingPass.Metadata> restoreFromSerialization(CompilerStub compiler) {
    return Option.apply(this);
  }

  @Override
  public final Option duplicate() {
    return Option.empty();
  }

  public abstract static sealed class Scope extends AliasMetadata permits RootScope, ChildScope {
    private Scope(Graph g) {
      super(g);
    }
  }

  /**
   * Aliasing information for a root scope.
   *
   * <p>A root scope has a 1:1 correspondence with a top-level binding.
   *
   * @param graph the graph containing the alias information for that node
   */
  public static final class RootScope extends Scope {
    public RootScope(Graph graph) {
      super(graph);
    }

    @Override
    public String metadataName() {
      return "AliasMetadata.RootScope";
    }

    public final RootScope copy(Graph graph) {
      return new RootScope(graph);
    }
  }

  /** Aliasing information about a child scope. */
  public static final class ChildScope extends Scope {
    private final Graph.Scope scope;

    /**
     * Aliasing information about a child scope.
     *
     * @param b the graph builder
     */
    public static ChildScope from(GraphBuilder b) {
      return new ChildScope(b.toGraph(), b.toScope());
    }

    /**
     * Aliasing information about a child scope.
     *
     * @param graph the graph
     * @param scope the child scope in `graph`
     */
    public ChildScope(Graph graph, Graph.Scope scope) {
      super(graph);
      this.scope = scope;
    }

    public final Graph.Scope scope() {
      return scope;
    }

    public final ChildScope copy(Graph graph, Graph.Scope scope) {
      return new ChildScope(graph, scope);
    }

    @Override
    public String metadataName() {
      return "AliasMetadata.ChildScope";
    }
  }

  /** Aliasing information for a piece of [[IR]] that is contained within a [[Scope]]. */
  public static final class Occurrence extends AliasMetadata {
    private final int id;

    /**
     * Aliasing information for a piece of [[IR]] that is contained within a [[Scope]].
     *
     * @param graph the graph in which this IR node can be found
     * @param id the identifier of this IR node in `graph`
     */
    public Occurrence(Graph graph, int id) {
      super(graph);
      this.id = id;
    }

    @Override
    public String metadataName() {
      return "AliasMetadata.Occurrence";
    }

    public final Occurrence copy(Graph graph, int id) {
      return new Occurrence(graph, id);
    }

    public final Occurrence copy(Graph graph) {
      return new Occurrence(graph, this.id);
    }

    public int id() {
      return id;
    }
  }
}
