package org.enso.compiler.core.ir

import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.core.ir.ProcessingPass
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.CompilerStub

/** Stores metadata for the various passes.
  *
  * @param startingMeta metadata mappings to initialise the configuration
  *                     storage with
  */
//noinspection DuplicatedCode
final class MetadataStorage(
  private var metadata: Map[ProcessingPass, Any]
) {
  def this(startingMeta: Seq[MetadataPair[_]] = Seq()) = {
    this(
      Map(
        startingMeta.map(_.asPair.asInstanceOf[(ProcessingPass, Any)]): _*
      )
    )
  }

  /** Adds a metadata pair to the node metadata.
    *
    * This will overwrite any entry whose key matches [[MetadataPair#pass]].
    *
    * @param metadataPair the pair to add to the storage
    * @tparam K the concrete type of the pass
    */
  def update[K <: ProcessingPass](metadataPair: MetadataPair[K]): Unit = {
    update(metadataPair.pass)(metadataPair.metadata)
  }

  /** Adds a new metadata entity to the pass metadata, or updates it if it
    * already exists for a given pass.
    *
    * @param pass the pass to add the metadata for
    * @param newMeta the metadata to add for `pass`
    * @tparam K the concrete type of `pass`
    */
  def update[K <: ProcessingPass](pass: K)(newMeta: pass.Metadata): Unit = {
    metadata = metadata + (pass -> newMeta)
  }

  /** Removes the metadata for the specified pass from the list.
    *
    * @param pass the pass to remove metadata for
    * @tparam K the concrete type of `pass`
    * @return the removed metadata for that pass, if it exists
    */
  def remove[K <: ProcessingPass](pass: K): Option[pass.Metadata] = {
    if (metadata.contains(pass)) {
      val res = get(pass)
      metadata = metadata.filter(t => t._1 != pass)
      res
    } else {
      None
    }
  }

  /** Gets the metadata for the specified pass.
    *
    * @param pass the pass to get the metadata for
    * @tparam K the concrete type of `pass`
    * @return the metadata for `pass`, if it exists
    */
  def get[K <: ProcessingPass](pass: K): Option[pass.Metadata] = {
    metadata.get(pass).map(_.asInstanceOf[pass.Metadata])
  }

  /** Unsafely gets the metadata for the specified pass, if it exists.
    *
    * @param pass the pass to get metadata for
    * @param msg the message to throw with if the unsafe get fails
    * @tparam K the concrete type of `pass`
    * @throws CompilerError if no metadata exists for `pass`
    * @return the metadata for `pass`, if it exists
    */
  def getUnsafe[K <: ProcessingPass](
    pass: K
  )(msg: => String = s"Missing metadata for pass $pass"): pass.Metadata = {
    get(pass).getOrElse(throw new CompilerError(msg))
  }

  /** Compares to pass metadata stores for equality.
    *
    * @param obj the object to compare against
    * @return `true` if `this == obj`, otherwise `false`
    */
  override def equals(obj: Any): Boolean =
    obj match {
      case that: MetadataStorage => this.metadata == that.metadata
      case _                     => false
    }

  /** Maps across the stored metadata, transforming it to an output map.
    *
    * @param f the function to apply over the metadata
    * @tparam K the output key type
    * @tparam V the output value type
    * @return a map containing the results of transforming the metadata storage
    */
  def map[K, V](
    f: (ProcessingPass, ProcessingPass.Metadata) => (K, V)
  ): Map[K, V] = {
    metadata
      .asInstanceOf[Map[ProcessingPass, ProcessingPass.Metadata]]
      .map(f.tupled)
  }

  /** Prepares the metadata for serialization.
    *
    * This operation takes place _in place_.
    *
    * Metadata prepared for serialization should not contain any links that
    * span more than one module, or any other properties that are problematic
    * when serialized.
    *
    * Due to the type safety properties of
    * [[org.enso.compiler.core.ir.MetadataStorage]], to allow this conversion
    * to work it must be type-refined to return `typeof this`. To that end,
    * there is no default definition for this method.
    *
    * @param compiler the Enso compiler
    */
  def prepareForSerialization(compiler: CompilerStub): Unit = {
    this.metadata = metadata.map { case (pass, value) =>
      val metadata = value.asInstanceOf[ProcessingPass.Metadata]
      val newVal = metadata
        // HP: could avoid casting by wrapping Metadata with some global compiler reference
        .prepareForSerialization(compiler.asInstanceOf[metadata.Compiler])
      (pass, newVal)
    }
  }

  /** Restores metadata after it has been deserialized.
    *
    * Due to the type safety properties of
    * [[org.enso.compiler.core.ir.MetadataStorage]], to allow this conversion
    * to work it must be type-refined to return `typeof this`. To that end,
    * there is no default definition for this method.
    *
    * @param compiler the Enso compiler
    * @return `true` if restoration was successful, `false` otherwise
    */
  def restoreFromSerialization(compiler: CompilerStub): Boolean = {
    this.metadata = metadata.map { case (pass, value) =>
      val metadata = value.asInstanceOf[ProcessingPass.Metadata]
      val meta = metadata
        .restoreFromSerialization(compiler.asInstanceOf[metadata.Compiler])
        .getOrElse(return false)
      (pass, meta)
    }
    true
  }

  /** Creates a copy of `this`.
    *
    * @return a copy of `this`
    */
  def copy: MetadataStorage = {
    val res = new MetadataStorage
    res.metadata = this.metadata
    res
  }

  override def toString: String = metadata.toString()

  /** Creates a deep copy of `this`.
    *
    * @return a deep copy of `this`
    */
  def duplicate: MetadataStorage = {
    val res = MetadataStorage()
    res.metadata = for {
      (pass, meta) <- this.metadata
      duplicated   <- meta.asInstanceOf[ProcessingPass.Metadata].duplicate()
    } yield (pass, duplicated)

    res
  }
}
object MetadataStorage extends MetadataStorageSyntax {

  /** Creates a new pass metadata safely.
    *
    * @param pairs the pairs of (pass, metadata)
    * @return a new [[MetadataStorage]]
    */
  def apply(pairs: MetadataPair[_]*): MetadataStorage = {
    new MetadataStorage(pairs)
  }

  /** A dependent pair for storing a pass and its metadata.
    *
    * @tparam P the concrete pass type
    */
  sealed trait MetadataPair[P <: ProcessingPass] {

    /** The pass itself. */
    val pass: P

    /** The metadata instance for [[pass]]. */
    val metadata: pass.Metadata

    /** Creates a string representation of the dependent pair.
      *
      * @return a string representation of `this`
      */
    override def toString: String =
      s"ConfigPair(pass: $pass, config: $metadata)"

    /** Determines whether two config pairs are equal.
      *
      * @param obj the object to check for equality against `this`
      * @return `true` if `this == obj`, otherwise `false`
      */
    override def equals(obj: Any): Boolean =
      obj match {
        case that: MetadataPair[_] =>
          (this.pass == that.pass) && (this.metadata == that.metadata)
        case _ => false
      }

    /** Converts the dependent pair into a standard pair ([[Tuple2]]).
      *
      * @return `this` as a pair
      */
    def asPair: (pass.type, pass.Metadata) = (pass, metadata)
  }
  object MetadataPair {

    /** Constructs a new metadata pair from a pass and a metadata instance for
      * that pass.
      *
      * @param newPass the pass
      * @param newMetadata the metadata for `pass`
      * @tparam P the concrete type of `newPass`
      * @return a metadata pair containing `newPass` and `configuration`
      */
    def apply[P <: ProcessingPass](newPass: P)(
      newMetadata: newPass.Metadata
    ): MetadataPair[newPass.type] = {
      new MetadataPair[newPass.type] {
        val pass: newPass.type      = newPass
        val metadata: pass.Metadata = newMetadata
      }
    }
  }
}
trait MetadataStorageSyntax {

  /** Adds an extension method on passes for concatenating them into pairs with
    * metadata for the pass.
    *
    * @param pass the pass to create a pair with
    * @tparam P the concrete type of `pass`
    */
  implicit final class ToPair[P <: ProcessingPass](val pass: P) {

    /** Concatenates [[pass]] with a metadata object for that pass.
      *
      * @param metadata the configuration to turn into a pair
      * @return the pair of ([[pass]], `metadata`)
      */
    def -->>(metadata: pass.Metadata): MetadataPair[pass.type] = {
      MetadataPair(pass)(metadata)
    }
  }
}
