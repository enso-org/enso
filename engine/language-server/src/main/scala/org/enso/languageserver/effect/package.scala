package org.enso.languageserver

import zio._
import zio.blocking.Blocking

package object effect {

  type BlockingIO[+E, +A] = ZIO[Blocking, E, A]
}
