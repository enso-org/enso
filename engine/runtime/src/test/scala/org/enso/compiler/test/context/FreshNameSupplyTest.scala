package org.enso.compiler.test.context

import org.enso.compiler.context.FreshNameSupply
import org.enso.compiler.core.ir.Name
import org.enso.compiler.test.CompilerTest

import scala.collection.mutable

class FreshNameSupplyTest extends CompilerTest {

  "The fresh name supply" should {
    val fns = new FreshNameSupply

    "generate unique identifiers" in {
      val seenNames: mutable.Set[Name] = mutable.Set()

      for (_ <- 1 to 100000) {
        val newName = fns.newName()

        if (seenNames contains newName) {
          fail()
        }

        succeed
      }
    }
  }
}
