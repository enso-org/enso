package org.enso.compiler.pass

import java.util.UUID

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR

import scala.collection.mutable

/** The pass manager is responsible for executing the provided passes in order.
  *
  * @param passOrdering the specification of the ordering for the passes
  * @param passConfiguration the configuration for the passes
  */
//noinspection DuplicatedCode
class PassManager(
  passOrdering: List[IRPass],
  passConfiguration: PassConfiguration
) {
  sealed case class PassCount(available: Int = 1, completed: Int = 0)

  /** Calculates the number of times each pass occurs in the pass ordering.
    *
    * @return the a mapping from the pass identifier to the number of times the
    *         pass occurs
    */
  def calculatePassCounts: mutable.Map[UUID, PassCount] = {
    val passCounts: mutable.Map[UUID, PassCount] = mutable.Map()

    for (pass <- passOrdering) {
      passCounts.get(pass.key) match {
        case Some(counts) =>
          passCounts(pass.key) = counts.copy(available = counts.available + 1)
        case None => passCounts(pass.key) = PassCount()
      }
    }

    passCounts
  }

  /** Executes the passes on an [[IR.Module]].
    *
    * @param ir the module to execute the compiler phases on
    * @param moduleContext the module context in which the phases are executed
    * @return the result of executing [[passOrdering]] on `ir`
    */
  def runPassesOnModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val passCounts = calculatePassCounts

    val newContext =
      moduleContext.copy(passConfiguration = Some(passConfiguration))

    passOrdering.foldLeft(ir)((intermediateIR, pass) => {
      val passCount = passCounts(pass.key)

      passConfiguration
        .get[pass.Config](pass)
        .foreach(c =>
          c.shouldWriteToContext =
            passCount.available - passCount.completed == 1
        )

      val result = pass.runModule(intermediateIR, newContext)

      passCounts(pass.key) = passCount.copy(completed = passCount.completed + 1)

      result
    })
  }

  /** Executes the passes on an [[IR.Expression]].
    *
    * @param ir the expression to execute the compiler phases on
    * @param inlineContext the inline context in which the expression is
    *                      processed
    * @return the result of executing [[passOrdering]] on `ir`
    */
  def runPassesInline(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    val passCounts = calculatePassCounts

    val newContext =
      inlineContext.copy(passConfiguration = Some(passConfiguration))

    passOrdering.foldLeft(ir)((intermediateIR, pass) => {
      val passCount = passCounts(pass.key)

      passConfiguration
        .get[pass.Config](pass)
        .foreach(c =>
          c.shouldWriteToContext =
            passCount.available - passCount.completed == 1
        )

      val result = pass.runExpression(intermediateIR, newContext)

      passCounts(pass.key) = passCount.copy(completed = passCount.completed + 1)

      result
    })
  }
}
