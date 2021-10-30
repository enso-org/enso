package org.enso.polyglot.debugger

/** A user-friendly wrapper for the internal object representation used in the
  * binary protocol
  */
class ObjectRepresentation(val representation: protocol.ObjectRepresentation)
    extends AnyVal {
  override def toString: String = representation.representation()
}
