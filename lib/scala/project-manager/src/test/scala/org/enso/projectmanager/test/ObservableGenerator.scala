package org.enso.projectmanager.test

import java.util.UUID

import org.enso.projectmanager.infrastructure.random.Generator
import zio.{IO, ZIO}

class ObservableGenerator[R] extends Generator[ZIO[R, +*, +*]] {

  private var buffer = Vector.empty[UUID]

  override def randomUUID(): IO[Nothing, UUID] =
    ZIO.succeed {
      val uuid = UUID.randomUUID()
      this.synchronized {
        buffer = buffer :+ uuid
        this.notifyAll()
      }
      uuid
    }

  def takeFirst(): UUID = {
    this.synchronized {
      while (buffer.headOption.isEmpty) {
        this.wait(100)
      }
      val head = buffer.head
      buffer = buffer.tail
      head
    }
  }

  def reset(): Unit = {
    this.synchronized {
      buffer = Vector()
    }
  }

}
