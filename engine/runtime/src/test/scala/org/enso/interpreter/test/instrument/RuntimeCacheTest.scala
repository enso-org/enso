package org.enso.interpreter.test.instrument

import java.util
import java.util.UUID

import org.enso.interpreter.instrument.RuntimeCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.jdk.CollectionConverters._

class RuntimeCacheTest extends AnyFlatSpec with Matchers {

  "RutimeCache" should "cache items" in {
    val cache = new RuntimeCache
    val key   = UUID.randomUUID
    val obj   = 42

    cache.offer(key, obj) shouldEqual false
    cache.get(key) shouldEqual null

    cache.setWeights(
      Map(key -> 1.0).asJava.asInstanceOf[util.Map[UUID, java.lang.Double]]
    )
    cache.offer(key, obj) shouldEqual true
    cache.get(key) shouldEqual obj
  }

  it should "remove items" in {
    val cache = new RuntimeCache
    val key   = UUID.randomUUID
    val obj   = new Object

    cache.setWeights(
      Map(key -> 1.0).asJava.asInstanceOf[util.Map[UUID, java.lang.Double]]
    )
    cache.offer(key, obj) shouldEqual true
    cache.remove(key) shouldEqual obj
    cache.get(key) shouldEqual null
  }
}
