package org.enso.compiler.pass.analyse.alias.graph;

import java.util.UUID;
import org.enso.compiler.core.ExternalID;
import org.enso.compiler.core.Identifier;
import org.enso.persist.Persistable;
import scala.Option;

/**
 * An occurrence of a given symbol in the aliasing graph. Note that this is not present in the
 * metadata attached to the [[org.enso.compiler.core.IR]] elements, but only in the alias [[Graph]].
 */
public abstract sealed class GraphOccurrence permits GraphOccurrence.Def, GraphOccurrence.Use {
  static Use createUse(
      ScopeImpl scope, int nextId, String symbol, UUID identifier, Option<UUID> externalId) {
    return new Use(scope, nextId, symbol, identifier, externalId);
  }

  public abstract int id();

  public abstract String symbol();

  abstract GraphOccurrence withScope(ScopeImpl scope);

  public abstract Graph.Scope scope();

  /** The definition of a symbol in the aliasing graph. */
  @Persistable(id = 1265, allowInlining = false)
  public static final class Def extends GraphOccurrence {
    private final ScopeImpl scope;
    private final int id;
    private final int slotIndx;
    private final String symbol;
    private final @Identifier UUID identifier;
    private final @ExternalID UUID externalId;
    private final boolean isLazy;

    private Def(
        ScopeImpl scope,
        int id,
        int slotIndx,
        String symbol,
        UUID identifier,
        scala.Option<UUID> externalId,
        boolean isLazy) {
      this.scope = scope;
      this.id = id;
      this.slotIndx = slotIndx;
      this.externalId = externalId.nonEmpty() ? externalId.get() : null;
      this.identifier = identifier;
      this.isLazy = isLazy;
      this.symbol = symbol;
    }

    /**
     * The definition of a symbol in the aliasing graph.
     *
     * @param id the identifier of the name in the graph
     * @param symbol the text of the name
     * @param identifier the identifier of the symbol
     * @param externalId the external identifier for the IR node defining the symbol
     * @param isLazy whether or not the symbol is defined as lazy
     */
    Def(
        int id,
        int slotIndx,
        String symbol,
        UUID identifier,
        scala.Option<UUID> externalId,
        boolean isLazy) {
      this(null, id, slotIndx, symbol, identifier, externalId, isLazy);
    }

    @Override
    Def withScope(ScopeImpl scope) {
      return new Def(scope, id, slotIndx, symbol, identifier, Option.apply(externalId), isLazy);
    }

    @Override
    public Graph.Scope scope() {
      return scope;
    }

    @Override
    public int id() {
      return this.id;
    }

    public final int slotIndx() {
      return slotIndx;
    }

    @Override
    public String symbol() {
      return this.symbol;
    }

    public UUID identifier() {
      return identifier;
    }

    public scala.Option<UUID> externalId() {
      return scala.Option.apply(externalId);
    }

    public boolean isLazy() {
      return isLazy;
    }

    @Override
    public String toString() {
      return "Def{" + "id=" + id + ", symbol=" + symbol + ", isLazy=" + isLazy + '}';
    }

    public static scala.Option<scala.Tuple5<Integer, String, UUID, scala.Option<UUID>, Boolean>>
        unapply(Object obj) {
      if (obj instanceof Def d) {
        var extId = scala.Option.apply(d.externalId);
        var tuple = new scala.Tuple5<>(d.id, d.symbol, d.identifier, extId, d.isLazy);
        return scala.Option.apply(tuple);
      }
      return scala.Option.empty();
    }
  }

  /** A usage of a symbol in the aliasing graph */
  @Persistable(id = 1264, allowInlining = false)
  public static final class Use extends GraphOccurrence {
    private final int id;
    private final ScopeImpl scope;
    private final String symbol;
    private final @Identifier UUID identifier;
    private final @ExternalID UUID externalId;

    /**
     * A usage of a symbol in the aliasing graph
     *
     * <p>Name usages _need not_ correspond to name definitions, as dynamic symbol resolution means
     * that a name used at runtime _may not_ be statically visible in the scope.
     *
     * @param id the identifier of the name in the graph
     * @param symbol the text of the name
     * @param identifier the identifier of the symbol
     * @param externalId the external identifier for the IR node defining the symbol
     */
    private Use(
        ScopeImpl scope, int id, String symbol, UUID identifier, scala.Option<UUID> externalId) {
      this.scope = scope;
      this.id = id;
      this.symbol = symbol;
      this.externalId = externalId.nonEmpty() ? externalId.get() : null;
      this.identifier = identifier;
    }

    Use(int id, String symbol, UUID identifier, scala.Option<UUID> externalId) {
      this(null, id, symbol, identifier, externalId);
    }

    @Override
    final Use withScope(ScopeImpl scope) {
      return new Use(scope, id, symbol, identifier, Option.apply(externalId));
    }

    @Override
    public Graph.Scope scope() {
      return scope;
    }

    @Override
    public int id() {
      return this.id;
    }

    @Override
    public String symbol() {
      return this.symbol;
    }

    public UUID identifier() {
      return identifier;
    }

    public scala.Option<UUID> externalId() {
      return scala.Option.apply(externalId);
    }

    @Override
    public String toString() {
      return "Use{" + "id=" + id + ", symbol=" + symbol + '}';
    }

    public static scala.Option<scala.Tuple4<Integer, String, UUID, scala.Option<UUID>>> unapply(
        Object obj) {
      if (obj instanceof Use u) {
        var extId = scala.Option.apply(u.externalId);
        var tuple = new scala.Tuple4<>(u.id, u.symbol, u.identifier, extId);
        return scala.Option.apply(tuple);
      }
      return scala.Option.empty();
    }
  }
}
