package org.enso.languageserver.runtime

import java.util.UUID

/**
  * An update containing information about expression.
  *
  * @param id expression id
  * @param type optional type of expression
  * @param shortValue optional value of expression
  * @param methodCall optional pointer to a method definition
  */
case class ExpressionValueUpdate(
  id: UUID,
  `type`: Option[String],
  shortValue: Option[String],
  methodCall: Option[MethodPointer]
)
