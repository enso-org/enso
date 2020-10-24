package org.enso.languageserver.effect

import scala.concurrent.Await
import scala.concurrent.duration._

/** Functions helping to manage effects in tests.
  */
trait Effects {

  implicit final class UnsafeRunZio[E, A](io: zio.ZIO[zio.ZEnv, E, A]) {
    def unsafeRunSync(): Either[E, A] =
      Await.result(ZioExec(zio.Runtime.default).exec(io), 3.seconds)
  }
}

object Effects extends Effects
