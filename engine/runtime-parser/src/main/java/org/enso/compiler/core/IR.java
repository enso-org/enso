package org.enso.compiler.core;

import java.io.Serializable;
import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.syntax.text.Debug;
import scala.Option;
import scala.collection.immutable.$colon$colon$;
import scala.collection.immutable.List;

/**
 * [[IR]] is a temporary and fairly unsophisticated internal representation format for Enso
 * programs.
 *
 * <p>It is a purely tree-based representation to support basic desugaring and analysis passes that
 * do not rely on the ability to create cycles in the IR itself. Its existence is the natural
 * evolution of the older AstExpression format used during the initial development of the
 * interpreter.
 *
 * <p>Please note that all extensions of [[IR]] must reimplement `copy` to keep the id intact when
 * copying nodes. The copy implementation should provide a way to set the id for the copy, but
 * should default to being copied. Care must be taken to not end up with two nodes with the same ID.
 * When using `copy` to duplicate nodes, please ensure that a new ID is provided.
 *
 * <p>See also: Note [IR Equality and hashing]
 */
public interface IR extends Serializable {

  long serialVersionUID = 8145L; // Scala to Java

  /**
   * Storage for metadata that the node has been tagged with as the result of various compiler
   * passes.
   */
  MetadataStorage passData();

  /** The source location that the node corresponds to. */
  Option<IdentifiedLocation> location();

  /**
   * Sets the location for an IR node.
   *
   * @param location the new location for the IR node
   * @return the IR node with its location set to `location`
   */
  IR setLocation(Option<IdentifiedLocation> location);

  /**
   * Gets the external identifier from an IR node, if it is present.
   *
   * @return the external identifier for this IR node
   */
  default Option<@ExternalID UUID> getExternalId() {
    return location().flatMap(l -> l.id().map(id -> id));
  }

  /**
   * Maps the provided function over any expression defined as a child of the node this is called
   * on.
   *
   * @param fn the function to transform the expressions
   * @return `this`, potentially having had its children transformed by `fn`
   */
  IR mapExpressions(Function<Expression, Expression> fn);

  /**
   * Gets the list of all children IR nodes of this node.
   *
   * @return this node's children.
   */
  List<IR> children();

  /**
   * Lists all the nodes in the preorder walk of the tree of this node.
   *
   * @return all the descendants of this node.
   */
  default List<IR> preorder() {
    List<IR> ordered = children().flatMap(c -> c.preorder());
    IR element = this;
    return $colon$colon$.MODULE$.apply(
        element, ordered); // ordered.prepended(element) is reporeted as ambiguous
  }

  /**
   * Pretty prints the IR.
   *
   * @return a pretty-printed representation of the IR
   */
  default String pretty() {
    return Debug.pretty(this.toString());
  }

  /**
   * Gets the node's identifier.
   *
   * @return the node's identifier
   */
  @Identifier
  UUID getId();

  /** Storage for compiler diagnostics related to the IR node. */
  DiagnosticStorage diagnostics();

  /**
   * Creates a deep structural copy of `this`, representing the same structure.
   *
   * <p>You can choose to keep the location, metadata and diagnostic information in the duplicated
   * copy, as well as whether you want to generate new node identifiers or not.
   *
   * @param keepLocations whether locations should be kept in the duplicated IR
   * @param keepMetadata whether the pass metadata should be kept in the duplicated IR
   * @param keepDiagnostics whether the diagnostics should be kept in the duplicated IR
   * @param keepIdentifiers whether the identifiers should be regenerated in the duplicated IR
   * @return a deep structural copy of `this`
   */
  IR duplicate(
      boolean keepLocations,
      boolean keepMetadata,
      boolean keepDiagnostics,
      boolean keepIdentifiers);

  /**
   * Shows the IR as code.
   *
   * @param indent the current indentation level
   * @return a string representation of `this`
   */
  String showCode(int indent);

  default String showCode() {
    return showCode(0);
  }

  /**
   * Creates a random identifier.
   *
   * @return a random identifier
   */
  static @Identifier UUID randomId() {
    return UUID.randomUUID();
  }

  /**
   * Generates an indent of `n` spaces.
   *
   * @param n the number of spaces
   * @return a string representing an `n`-space indent
   */
  static String mkIndent(int n) {
    return " ".repeat(n);
  }

  /** The size of a single indentation level. */
  int indentLevel = 4;
}
