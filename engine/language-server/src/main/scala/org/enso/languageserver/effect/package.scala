package org.enso.languageserver

import zio._

package object effect {

  type BlockingIO[+E, +A] = ZIO[ZAny, E, A]
}
