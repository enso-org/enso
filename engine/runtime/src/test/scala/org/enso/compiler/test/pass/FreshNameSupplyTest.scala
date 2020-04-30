package org.enso.compiler.test.pass

import org.enso.compiler.context.FreshNameSupply
import org.enso.compiler.core.IR
import org.enso.compiler.test.CompilerTest

import scala.collection.mutable

class FreshNameSupplyTest extends CompilerTest {

  "The fresh name supply" should {
    val fns = new FreshNameSupply

    "generate unique identifiers" in {
      val seenNames: mutable.Set[IR.Name] = mutable.Set()

      for (_ <- 1 to 100000) {
        val newName = fns.newName()

        if (seenNames contains newName) {
          fail
        }

        succeed
      }
    }
  }
}
