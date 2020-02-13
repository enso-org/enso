package org.enso.compiler.test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait CompilerRunner {}

trait CompilerTest extends AnyFlatSpec with Matchers with CompilerRunner
