package org.enso.interpreter.test.instrument

import java.util.UUID

import org.enso.interpreter.instrument.RuntimeCache
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RuntimeCacheTest extends AnyFlatSpec with Matchers {

  "RutimeCache" should "cache items" in {
    val cache = new RuntimeCache
    val key = UUID.randomUUID
    val obj = 42

    cache.put(key, obj) shouldEqual null
    cache.get(key) shouldEqual 42
  }

  it should "remove items" in {
    val cache = new RuntimeCache
    val key = UUID.randomUUID
    val obj = new Object

    cache.put(key, obj)
    cache.remove(key) shouldEqual obj
    cache.get(key) shouldEqual null
  }
}
