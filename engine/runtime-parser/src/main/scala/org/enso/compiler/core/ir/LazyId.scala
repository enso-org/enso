package org.enso.compiler.core.ir

import org.enso.compiler.core.IR.randomId
import org.enso.compiler.core.{IR, Identifier}

import java.util.UUID

trait LazyId { self: IR =>
  private[this] var _id: UUID @Identifier = null

  protected def id: UUID @Identifier = {
    _id
  }

  override def getId(): UUID @Identifier = {
    if (_id == null) {
      _id = randomId()
    }
    _id
  }

  def id_=(id: UUID @Identifier) = {
    assert(_id == null)
    _id = id
  }
}
