package org.enso.syntax

import org.enso.data.Span
import java.util.UUID

object Parser {
  type IDMap = Seq[(Span, UUID)]
}
