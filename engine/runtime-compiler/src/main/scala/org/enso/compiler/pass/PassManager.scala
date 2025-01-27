package org.enso.compiler.pass

import org.slf4j.LoggerFactory
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{Expression, Module}
import org.enso.compiler.core.{CompilerError, IR}
import org.enso.compiler.dump.service.IRDumper

import java.io.File
import scala.collection.mutable.ListBuffer

// TODO [AA] In the future, the pass ordering should be _computed_ from the list
//  of available passes, rather than just verified.

/** A manager for compiler passes.
  *
  * It is responsible for verifying and executing passes (in groups) on the
  * compiler IR.
  *
  * @param passes the pass groups, must all be unique
  * @param passConfiguration the configuration for each pass in `passes`
  */
//noinspection DuplicatedCode
class PassManager(
  protected val passes: List[PassGroup],
  passConfiguration: PassConfiguration
) {
  private val logger = LoggerFactory.getLogger(classOf[PassManager])
  val allPasses      = verifyPassOrdering(passes.flatMap(_.passes))

  /** Computes a valid pass ordering for the compiler.
    *
    * @param passes the input list of passes
    * @throws CompilerError if a valid pass ordering cannot be computed
    * @return a valid pass ordering for the compiler, based on `passes`
    */
  private def verifyPassOrdering(
    passes: List[IRProcessingPass]
  ): List[IRProcessingPass] = {
    var validPasses: Set[IRProcessingPass] = Set()

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

  /** Executes the provided `passGroup` on the [[Module]].
    *
    * @param ir the module to execute the compiler passes on
    * @param moduleContext the module context in which the passes are executed
    * @param passGroup the group of passes being executed
    * @return the result of executing `passGroup` on `ir`
    */
  def runPassesOnModule(
    ir: Module,
    moduleContext: ModuleContext,
    passGroup: PassGroup,
    irDumper: Option[IRDumper]
  ): Module = {
    if (!passes.contains(passGroup)) {
      throw new CompilerError("Cannot run an unvalidated pass group.")
    }

    logger.debug(
      "runPassesOnModule[{}@{}]",
      moduleContext.getName(),
      moduleContext.module.getCompilationStage()
    )

    val newContext =
      moduleContext.copy(passConfiguration = Some(passConfiguration))

    def getSrcFile(): File = {
      val modPath = newContext.module.getPath
      if (modPath == null) {
        null
      } else {
        new File(modPath)
      }
    }

    runPasses[Module, ModuleContext](
      ir,
      newContext,
      passGroup,
      moduleName = Some(moduleContext.getName().toString),
      irDumper   = irDumper,
      getSrcFile = getSrcFile,
      createMiniPass =
        (factory, ctx) => factory.createForModuleCompilation(ctx),
      miniPassCompile = (miniPass, ir) =>
        MiniIRPass.compile[Module](classOf[Module], ir, miniPass),
      megaPassCompile = (megaPass, ir, ctx) => megaPass.runModule(ir, ctx)
    )
  }

  /** Executes all passes on the [[Expression]].
    *
    * @param ir the expression to execute the compiler passes on
    * @param inlineContext the inline context in which the passes are executed
    * @return the result of executing `passGroup` on `ir`
    */
  def runPassesInline(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    passes.foldLeft(ir)((ir, group) =>
      runPassesInline(ir, inlineContext, group)
    )
  }

  /** Executes the provided `passGroup` on the [[Expression]].
    *
    * @param ir the expression to execute the compiler passes on
    * @param inlineContext the inline context in which the passes are executed
    * @param passGroup the group of passes being executed
    * @return the result of executing `passGroup` on `ir`
    */
  def runPassesInline(
    ir: Expression,
    inlineContext: InlineContext,
    passGroup: PassGroup
  ): Expression = {
    if (!passes.contains(passGroup)) {
      throw new CompilerError("Cannot run an unvalidated pass group.")
    }

    val newContext =
      inlineContext.copy(passConfiguration = Some(passConfiguration))

    def getSrcFile(): File = {
      val modPath = inlineContext.getModule().getPath
      if (modPath == null) {
        null
      } else {
        new File(modPath)
      }
    }

    runPasses[Expression, InlineContext](
      ir,
      newContext,
      passGroup,
      moduleName = null,
      getSrcFile = getSrcFile,
      irDumper   = None,
      createMiniPass =
        (factory, ctx) => factory.createForInlineCompilation(ctx),
      miniPassCompile = (miniPass, ir) =>
        MiniIRPass.compile[Expression](classOf[Expression], ir, miniPass),
      megaPassCompile = (megaPass, ir, ctx) => megaPass.runExpression(ir, ctx)
    )
  }

  private def dump(
    ir: IR,
    moduleName: Option[String],
    irDumper: Option[IRDumper],
    passName: String,
    getSrcFile: () => File
  ): Unit = {
    (ir, moduleName, irDumper) match {
      case (moduleIr: Module, Some(modName), Some(dumper)) =>
        dumper.dumpModule(moduleIr, modName, getSrcFile(), passName)
      case _ => ()
    }
  }

  /** Runs all the passes in the given `passGroup` on `ir` with `context`.
    * @param createMiniPass Function that creates a minipass.
    * @param miniPassCompile Function that compiles IR with mini pass.
    * @param megaPassCompile Function that compiles IR with mega pass.
    * @tparam IRType Type of the [[IR]] that is being compiled.
    * @tparam ContextType Type of the context for the compilation.
    *                     Either [[ModuleContext]] or [[InlineContext]]
    * @return Compiled IR. Might be the same reference as `ir` if no compilation was done.
    */
  private def runPasses[IRType <: IR, ContextType](
    ir: IRType,
    context: ContextType,
    passGroup: PassGroup,
    moduleName: Option[String],
    irDumper: Option[IRDumper],
    getSrcFile: () => File,
    createMiniPass: (MiniPassFactory, ContextType) => MiniIRPass,
    miniPassCompile: (MiniIRPass, IRType) => IRType,
    megaPassCompile: (IRPass, IRType, ContextType) => IRType
  ): IRType = {
    val pendingMiniPasses: ListBuffer[MiniPassFactory] = ListBuffer()

    def flushMiniPasses(in: IRType): IRType = {
      if (pendingMiniPasses.nonEmpty) {
        val miniPasses =
          pendingMiniPasses.map(factory => createMiniPass(factory, context))
        val combinedPass = miniPasses.fold(null)(MiniIRPass.combine)
        pendingMiniPasses.clear()
        if (combinedPass != null) {
          logger.trace("  flushing pending mini pass: {}", combinedPass)
          val ret = miniPassCompile(combinedPass, in)
          dump(ret, moduleName, irDumper, combinedPass.toString, getSrcFile)
          ret
        } else {
          in
        }
      } else {
        in
      }
    }

    val passesWithIndex = passGroup.passes.zipWithIndex
    val res = passesWithIndex.foldLeft(ir) {
      case (intermediateIR, (pass, index)) =>
        pass match {
          case miniFactory: MiniPassFactory =>
            logger.trace(
              "  mini collected: {}",
              pass
            )
            val combiningPreventedByOpt = pendingMiniPasses.find { p =>
              p.invalidatedPasses.contains(miniFactory)
            }
            val irForRemainingMiniPasses = combiningPreventedByOpt match {
              case Some(combiningPreventedBy) =>
                logger.trace(
                  "  pass {} forces flush before (invalidates) {}",
                  combiningPreventedBy,
                  miniFactory
                )
                flushMiniPasses(intermediateIR)
              case None =>
                intermediateIR
            }
            pendingMiniPasses.addOne(miniFactory)
            irForRemainingMiniPasses

          case megaPass: IRPass =>
            // TODO [AA, MK] This is a possible race condition.
            passConfiguration
              .get(megaPass)
              .foreach(c =>
                c.shouldWriteToContext = isLastRunOf(index, megaPass, passGroup)
              )
            val flushedIR = flushMiniPasses(intermediateIR)
            logger.trace(
              "  mega running: {}",
              megaPass
            )
            val ret = megaPassCompile(megaPass, flushedIR, context)
            dump(ret, moduleName, irDumper, megaPass.toString, getSrcFile)
            ret
        }
    }

    flushMiniPasses(res)
  }

  /** Determines whether the run at index `indexOfPassInGroup` is the last run
    * of that pass in the overall pass ordering.
    *
    * @param indexOfPassInGroup the index of `pass` in `passGroup`
    * @param pass the pass to check for
    * @param passGroup the pass group being run
    * @return `true` if the condition holds, otherwise `false`
    */
  private def isLastRunOf(
    indexOfPassInGroup: Int,
    pass: IRPass,
    passGroup: PassGroup
  ): Boolean = {
    val ix          = allPasses.lastIndexOf(pass)
    val before      = passes.takeWhile(_ != passGroup)
    val totalLength = before.map(_.passes.length).sum

    ix - totalLength == indexOfPassInGroup
  }

  /** Updates the metadata in a copy of the IR when updating that metadata
    * requires global state.
    *
    * This is usually the case in the presence of structures that are shared
    * throughout the IR, and need to maintain that sharing for correctness. This
    * must be called with `copyOfIr` as the result of an `ir.duplicate` call.
    *
    * Additionally this method _must not_ alter the structure of the IR. It
    * should only update its metadata.
    *
    * @param sourceIr the IR being copied
    * @param copyOfIr a duplicate of `sourceIr`
    * @return the result of updating metadata in `copyOfIr` globally using
    *         information from `sourceIr`
    */
  def runMetadataUpdate(sourceIr: Module, copyOfIr: Module): Module = {
    allPasses.foldLeft(copyOfIr) {
      case (module, megaPass: IRPass) =>
        megaPass.updateMetadataInDuplicate(sourceIr, module)
      case (module, _) => module
    }
  }
}

/** A representation of a group of passes.
  *
  * @param passes the passes in the group
  */
class PassGroup(val passes: List[IRProcessingPass])
