package org.enso.languageserver.boot.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ApplicationConfigTest extends AnyFlatSpec with Matchers {

  it should "load config" in {
    ApplicationConfig.load()
  }
}
