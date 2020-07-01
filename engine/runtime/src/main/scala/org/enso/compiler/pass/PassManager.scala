package org.enso.compiler.pass

import java.util.UUID

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.exception.CompilerError

import scala.collection.mutable

// TODO [AA] In the future, the pass ordering should be _computed_ from the list
//  of available passes, rather than just verified.

/** The pass manager is responsible for executing the provided passes in order.
  *
  * @param passes the specification of the ordering for the passes
  * @param passConfiguration the configuration for the passes
  */
//noinspection DuplicatedCode
class PassManager(
  passes: List[IRPass],
  passConfiguration: PassConfiguration
) {
  val passOrdering: List[IRPass] = verifyPassOrdering(passes)

  /** Computes a valid pass ordering for the compiler.
    *
    * @param passes the input list of passes
    * @throws CompilerError if a valid pass ordering cannot be computed
    * @return a valid pass ordering for the compiler, based on `passes`
    */
  @throws[CompilerError]
  def verifyPassOrdering(passes: List[IRPass]): List[IRPass] = {
    var validPasses: Set[IRPass] = Set()

    passes.foreach(pass => {
      val prereqsSatisfied =
        pass.precursorPasses.forall(validPasses.contains(_))

      if (prereqsSatisfied) {
        validPasses += pass
      } else {
        val missingPrereqsStr =
          pass.precursorPasses.filterNot(validPasses.contains(_)).mkString(", ")

        throw new CompilerError(
          s"The pass ordering is invalid. $pass is missing valid results " +
          s"for: $missingPrereqsStr"
        )
      }

      pass.invalidatedPasses.foreach(p => validPasses -= p)
    })

    passes
  }

  /** Calculates the number of times each pass occurs in the pass ordering.
    *
    * @return the a mapping from the pass identifier to the number of times the
    *         pass occurs */
  private def calculatePassCounts: mutable.Map[UUID, PassCount] = {
    val passCounts: mutable.Map[UUID, PassCount] = mutable.Map()

    for (pass <- passOrdering) {
      passCounts.get(pass.key) match {
        case Some(counts) =>
          passCounts(pass.key) = counts.copy(expected = counts.expected + 1)
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
        .get(pass)
        .foreach(c =>
          c.shouldWriteToContext =
            passCount.expected - passCount.completed == 1
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
        .get(pass)
        .foreach(c =>
          c.shouldWriteToContext =
            passCount.expected - passCount.completed == 1
        )

      val result = pass.runExpression(intermediateIR, newContext)

      passCounts(pass.key) = passCount.copy(completed = passCount.completed + 1)

      result
    })
  }

  /** The counts of passes running.
    *
    * @param expected how many runs should occur
    * @param completed how many runs have been completed
    */
  sealed private case class PassCount(expected: Int = 1, completed: Int = 0)
}
