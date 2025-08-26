package org.enso.compiler.core;

import java.util.UUID;
import java.util.function.Consumer;
import java.util.function.Function;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.ProcessingPass.Metadata;
import org.enso.compiler.debug.Debug;
import scala.Option;
import scala.collection.immutable.List;

/**
 * {@link IR} is a temporary and fairly unsophisticated internal representation format for Enso
 * programs.
 *
 * <p>It is a purely tree-based representation to support basic desugaring and analysis passes that
 * do not rely on the ability to create cycles in the IR itself.
 *
 * <p>IR AST is generally immutable with the exception of {@link IR#passData() metadata} and {@link
 * IR#diagnostics() diagnostics} that are mutable.
 *
 * <p>The current IR hierarchy consist of old Scala case classes (for example {@link
 * org.enso.compiler.core.ir.Name.Qualified}) and of new Java classes annotated with {@link
 * org.enso.runtime.parser.dsl.GenerateIR} for which superclasses are generated. See {@link
 * org.enso.runtime.parser.dsl.GenerateIR} javadoc for more information on the generation of IR
 * classes. The desired state is to have the whole hierarchy in Java only.
 *
 * <h2>Copy</h2>
 *
 * The immutable nature of IR elements means that they cannot be modified in place. Modifications to
 * IR are done by copying the old element and returning a new one. Usually, <i>shallow</i> copies
 * are preferred.
 *
 * <h3>Shallow copy</h3>
 *
 * On old Scala case classes, various {@code copy} methods exist, like {@link
 * org.enso.compiler.core.ir.Name.Qualified#copy(List, Option, MetadataStorage, DiagnosticStorage,
 * UUID) Name.Qualified#copy}. The new Java classes also have generated {@code copy} methods as well
 * as generated copy builders.
 *
 * <p>All the fields that are not changed in the {@code copy} methods will contain references to the
 * original IR element. It the original IR element is not disposed and still referenced somewhere,
 * all the mutable structures on the subtree will potentially be modified from two places.
 * Therefore, <b>if you intend to keep the original IR element, use deep copy instead</b>.
 *
 * <p>When manually implementing {@code copy} methods (which is now discouraged with the
 * introduction of {@link org.enso.runtime.parser.dsl.GenerateIR}), special care must be taken to
 * keep the {@link IR#getId() UUID} intact. The copy implementation should provide a way to set the
 * UUID for the copy, but should default to being copied from the original element. <b>There should
 * never be two nodes with the same UUID</b>.
 *
 * <h3>Deep copy</h3>
 *
 * Deep copy is implemented by the {@link IR#duplicate(boolean, boolean, boolean, boolean)
 * duplicate} method. Note that special care must be taken when duplicating {@link IR#passData()
 * metadata}, as some {@link org.enso.compiler.core.ir.ProcessingPass compiler passes} may decide to
 * not duplicate the metadata at all by returning {@code None} from {@link Metadata#duplicate()}
 * method. In these cases, the newly duplicated IR element will have just a portion of the original
 * metadata.
 */
public interface IR {
  /**
   * Storage for metadata that the node has been tagged with as the result of various compiler
   * passes.
   */
  MetadataStorage passData();

  /**
   * The nullable source location that the node corresponds to.
   *
   * @return the node location or {@code null}
   */
  IdentifiedLocation identifiedLocation();

  /** The source location that the node corresponds to. */
  default Option<IdentifiedLocation> location() {
    return Option.apply(identifiedLocation());
  }

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
    return location().flatMap(IdentifiedLocation::id);
  }

  /**
   * Maps the provided function over any expression defined as a child of the node this is called
   * on. The child does not have to be a <emph>direct</emph> child, it can have an arbitrary number
   * of intermediate non-expression nodes. The mapping traverses in DFS order, and it stops on first
   * (non-direct) child that is an {@link Expression}.
   *
   * <p>The function is not applied on this IR, even if it is {@link Expression}.
   *
   * @param fn the function to transform the expressions
   * @return `this`, potentially having had its (non-direct) children transformed by `fn`
   */
  IR mapExpressions(Function<Expression, Expression> fn);

  /**
   * Gets the list of all children IR nodes of this node.
   *
   * @return this node's children.
   */
  List<IR> children();

  /**
   * Applies the callback to nodes in the preorder walk of the tree of this node.
   *
   * @param ir the node to traverse
   * @param cb the callback to apply
   */
  static void preorder(IR ir, Consumer<IR> cb) {
    final class CB implements scala.Function1<IR, scala.Unit> {
      @Override
      public scala.Unit apply(IR ir) {
        cb.accept(ir);
        ir.children().foreach(this);
        return null;
      }
    }

    cb.accept(ir);
    ir.children().foreach(new CB());
  }

  /**
   * Lists all the nodes in the preorder walk of the tree of this node.
   *
   * @return all the descendants of this node.
   */
  default List<IR> preorder() {
    var builder = new scala.collection.mutable.ListBuffer<IR>();
    IR.preorder(this, builder::addOne);

    return builder.result();
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
   * Gets the node's identifier. If there is none, a random {@link UUID} is generated.
   *
   * @return the node's identifier
   */
  @Identifier
  UUID getId();

  /**
   * Get the storage for compiler diagnostics related to the IR node.
   *
   * @return the diagnostic storage or {@code null} if not initialized
   */
  DiagnosticStorage diagnostics();

  /**
   * Initialize and get the storage for compiler diagnostics associated with this IR node.
   *
   * @return the diagnostic storage of this node. The result is never {@code null}
   */
  DiagnosticStorage getDiagnostics();

  /**
   * Creates a deep structural copy of `this`, representing the same structure.
   *
   * <p>You can choose to keep the location, metadata and diagnostic information in the duplicated
   * copy, as well as whether you want to generate new node identifiers or not.
   *
   * <p>Note that even with {@code keepMetadata} set to {@code true}, some compiler passes may
   * choose to not duplicate their metadata, returning {@code None} from their {@link
   * Metadata#duplicate()} method. In these cases, the newly duplicated IR will have just a portion
   * of the original metadata.
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
