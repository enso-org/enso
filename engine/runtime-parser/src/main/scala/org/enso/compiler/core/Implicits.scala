package org.enso.compiler.core

import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.{Diagnostic, ProcessingPass}

object Implicits {

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
      ir.passData.toString
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
      ir.passData.get(pass).asInstanceOf[Option[pass.Metadata]]
    }

    /** Unsafely gets the metadata for the specified pass, if it exists.
      *
      * @param pass the pass to get metadata for
      * @param msg  the message to throw with if the unsafe get fails
      * @tparam K the concrete type of `pass`
      * @throws CompilerError if no metadata exists for `pass`
      * @return the metadata for `pass`, if it exists
      */
    @throws[CompilerError]
    def unsafeGetMetadata[K <: ProcessingPass](
      pass: ProcessingPass,
      msg: => String
    ): pass.Metadata = {
      ir.passData
        .get(pass)
        .getOrElse(throw new CompilerError(msg))
        .asInstanceOf[pass.Metadata]
    }
  }

  /** Adds extension methods for working with lists of [[IR]].
    *
    * @param list the list
    * @tparam T the concrete IR type
    */
  implicit class ListAsIr[T <: IR](list: List[T]) {

    /** Calls [[IR#duplicate]] on the elements in [[list]].
      *
      * @param keepLocations   whether or not locations should be kept in the
      *                        duplicated IR
      * @param keepMetadata    whether or not the pass metadata should be kept in
      *                        the duplicated IR
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

}
