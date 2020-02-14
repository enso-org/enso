package org.enso.compiler.test

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

trait CompilerRunner {}

trait CompilerTest extends AnyWordSpecLike with Matchers with CompilerRunner
