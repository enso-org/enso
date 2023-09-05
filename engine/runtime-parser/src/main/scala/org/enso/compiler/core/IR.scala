package org.enso.compiler.core

import com.oracle.truffle.api.source.Source

import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.{
  Diagnostic,
  DiagnosticStorage,
  Expression,
  IdentifiedLocation,
  MetadataStorage,
  ProcessingPass
}
import org.enso.syntax.text.Debug

import java.util.UUID

/** [[IR]] is a temporary and fairly unsophisticated internal representation
  * format for Enso programs.
  *
  * It is a purely tree-based representation to support basic desugaring and
  * analysis passes that do not rely on the ability to create cycles in the IR
  * itself. Its existence is the natural evolution of the older AstExpression
  * format used during the initial development of the interpreter.
  *
  * In time, it will be replaced by [[Core]], but expediency dictates that we
  * retain and evolve this representation for the near future.
  *
  * Please note that all extensions of [[IR]] must reimplement `copy` to keep
  * the id intact when copying nodes. The copy implementation should provide a
  * way to set the id for the copy, but should default to being copied. Care
  * must be taken to not end up with two nodes with the same ID. When using
  * `copy` to duplicate nodes, please ensure that a new ID is provided.
  *
  * See also: Note [IR Equality and hashing]
  */
trait IR extends Serializable {

  /** Storage for metadata that the node has been tagged with as the result of
    * various compiler passes.
    */
  val passData: MetadataStorage

  /** The source location that the node corresponds to. */
  val location: Option[IdentifiedLocation]

  /** Sets the location for an IR node.
    *
    * @param location the new location for the IR node
    * @return the IR node with its location set to `location`
    */
  def setLocation(location: Option[IdentifiedLocation]): IR

  /** Gets the external identifier from an IR node, if it is present.
    *
    * @return the external identifier for this IR node
    */
  def getExternalId: Option[IR.ExternalId] = {
    location.flatMap(l => l.id)
  }

  /** Maps the provided function over any expression defined as a child of the
    * node this is called on.
    *
    * @param fn the function to transform the expressions
    * @return `this`, potentially having had its children transformed by `fn`
    */
  def mapExpressions(fn: Expression => Expression): IR

  /** Gets the list of all children IR nodes of this node.
    *
    * @return this node's children.
    */
  def children: List[IR]

  /** Lists all the nodes in the preorder walk of the tree of this node.
    *
    * @return all the descendants of this node.
    */
  def preorder: List[IR] = this :: children.flatMap(_.preorder)

  /** Pretty prints the IR.
    *
    * @return a pretty-printed representation of the IR
    */
  def pretty: String = Debug.pretty(this.toString)

  /** Gets the node's identifier.
    *
    * @return the node's identifier
    */
  def getId: IR.Identifier = id

  /** A unique identifier for a piece of IR. */
  protected var id: IR.Identifier

  /** Storage for compiler diagnostics related to the IR node. */
  val diagnostics: DiagnosticStorage

  /** Creates a deep structural copy of `this`, representing the same structure.
    *
    * You can choose to keep the location, metadata and diagnostic information
    * in the duplicated copy, as well as whether or not you want to generate new
    * node identifiers or not.
    *
    * @param keepLocations whether or not locations should be kept in the
    *                      duplicated IR
    * @param keepMetadata whether or not the pass metadata should be kept in the
    *                      duplicated IR
    * @param keepDiagnostics whether or not the diagnostics should be kept in
    *                        the duplicated IR
    * @param keepIdentifiers whether or not the identifiers should be
    *                        regenerated in the duplicated IR
    * @return a deep structural copy of `this`
    */
  def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): IR

  /** Shows the IR as code.
    *
    * @param indent the current indentation level
    * @return a string representation of `this`
    */
  def showCode(indent: Int = 0): String
}

/* Note [IR Equality and hashing]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * As the IRs are implemented as case classes, their equality is determined by
 * the values included in the constructor. These include the MetadataStorage and
 * DiagnosticStorage. These two storages break the contract of `hashCode` by
 * overriding the `equals` method to compare for equality by their contents, but
 * not `hashCode` (because it would have to be mutable). As the case classes of
 * the IR use that to implement their own equality and hashing, their
 * implementation is also troubled by this. Instances of IR that are equal by
 * the `equals` function, may still return different `hashCode`.
 *
 * The MetadataStorage and DiagnosticStorage should not be used for checking
 * equality of the IR. This should be addressed when the IR is refactored to be
 * properly mutable.
 */

object IR {

  /** Creates a random identifier.
    *
    * @return a random identifier
    */
  def randomId: IR.Identifier = {
    UUID.randomUUID()
  }

  /** The type of identifiers for IR nodes. */
  type Identifier = UUID

  /** The type of external identifiers */
  type ExternalId = UUID

  /** Generates an indent of `n` spaces.
    *
    * @param n the number of spaces
    * @return a string representing an `n`-space indent
    */
  def mkIndent(n: Int): String = {
    " " * n
  }

  /** The size of a single indentation level. */
  val indentLevel: Int = 4

  // ==========================================================================
  // === Extension Methods ====================================================
  // ==========================================================================

  /** This class adds an extension method to control how the pass data element
    * of the IR is printed.
    *
    * @param ir the IR to print the pass data for
    */
  implicit class ShowPassData(ir: IR) {

    /** Creates a string representation of the pass data for a given IR node.
      *
      * @return a string representation of the pass data for [[ir]]
      */
    def showPassData: String = {
      val metaString: Seq[String] =
        ir.passData.map((p, m) => (p, m.metadataName)).values.toSeq
      val alphabetical = metaString.sorted
      s"$alphabetical"
    }
  }

  /** Adds extension methods on strings to aid in writing custom to string
    * overrides.
    *
    * @param string the string to process
    */
  implicit class ToStringHelper(string: String) {

    /** Converts a multiline string to a single line
      *
      * @return [[string]], converted to a single line
      */
    def toSingleLine: String = {
      val lines = string.stripMargin.split("\n").toList.filterNot(_ == "")

      val body = lines.tail.dropRight(1).mkString(" ")

      s"${lines.head}$body${lines.last}"
    }
  }

  // ==========================================================================
  // === Useful Extension Methods =============================================
  // ==========================================================================

  /** Adds extension methods for working directly with the diagnostics on the
    * IR.
    *
    * @param ir the IR to add the methods to
    * @tparam T the concrete type of the IR
    */
  implicit class AsDiagnostics[T <: IR](ir: T) {

    /** Adds a new diagnostic entity to [[IR]].
      *
      * @param diagnostic the diagnostic to add
      * @return [[ir]] with added diagnostics
      */
    def addDiagnostic(diagnostic: Diagnostic): T = {
      ir.diagnostics.add(diagnostic)
      ir
    }
  }

  /** Adds extension methods for working directly with the metadata on the IR.
    *
    * @param ir the IR to add the methods to
    * @tparam T the concrete type of the IR
    */
  implicit class AsMetadata[T <: IR](ir: T) {

    /** Adds a metadata pair to the node metadata.
      *
      * This will overwrite any entry whose key matches [[MetadataPair#pass]].
      *
      * @param metadataPair the pair to add to the storage
      * @tparam K the concrete type of the pass
      */
    def updateMetadata[K <: ProcessingPass](
      metadataPair: MetadataPair[K]
    ): T = {
      ir.passData.update(metadataPair)
      ir
    }

    /** Gets the metadata for the specified pass.
      *
      * @param pass the pass to get the metadata for
      * @tparam K the concrete type of `pass`
      * @return the metadata for `pass`, if it exists
      */
    def getMetadata[K <: ProcessingPass](pass: K): Option[pass.Metadata] = {
      ir.passData.get(pass)
    }

    /** Unsafely gets the metadata for the specified pass, if it exists.
      *
      * @param pass the pass to get metadata for
      * @param msg the message to throw with if the unsafe get fails
      * @tparam K the concrete type of `pass`
      * @throws CompilerError if no metadata exists for `pass`
      * @return the metadata for `pass`, if it exists
      */
    @throws[CompilerError]
    def unsafeGetMetadata[K <: ProcessingPass](
      pass: ProcessingPass,
      msg: => String
    ): pass.Metadata = {
      ir.passData.getUnsafe(pass)(msg)
    }
  }

  /** Adds extension methods for working with lists of [[IR]].
    *
    * @param list the list
    * @tparam T the concrete IR type
    */
  implicit class ListAsIr[T <: IR](list: List[T]) {

    /** Calls [[IR.duplicate]] on the elements in [[list]].
      *
      * @param keepLocations whether or not locations should be kept in the
      *                      duplicated IR
      * @param keepMetadata whether or not the pass metadata should be kept in
      *                     the duplicated IR
      * @param keepDiagnostics whether or not the diagnostics should be kept in
      *                        the duplicated IR
      * @param keepIdentifiers whether or not the identifiers should be
      *                        regenerated in the duplicated IR
      * @return a duplicate of [[list]]
      */
    def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): List[T] = {
      list
        .map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        )
        .asInstanceOf[List[T]]
    }
  }

  def fileLocationFromSection(
    loc: IdentifiedLocation,
    source: Source
  ): String = {
    val section =
      source.createSection(loc.location.start, loc.location.length)
    val locStr =
      "" + section.getStartLine + ":" +
      section.getStartColumn + "-" +
      section.getEndLine + ":" +
      section.getEndColumn
    source.getName + "[" + locStr + "]"
  }
}
