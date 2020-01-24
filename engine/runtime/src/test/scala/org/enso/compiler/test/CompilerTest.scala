package org.enso.compiler.test

import org.scalatest.{FlatSpec, Matchers}

trait CompilerRunner {}

trait CompilerTest extends FlatSpec with Matchers with CompilerRunner
