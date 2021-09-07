package org.enso.compiler.pass.analyse

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.data.BindingsMap.{Resolution, ResolvedMethod}
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.ComplexType
import org.enso.compiler.pass.resolve.{
  IgnoredBindings,
  MethodCalls,
  ModuleAnnotations,
  TypeSignatures
}

import scala.annotation.{tailrec, unused}

/** This pass is responsible for discovering occurrences of automatically
  * parallelizable computations.
  *
  * It uses type signatures and annotations to drive the system. Each method
  * is split into blocks, where the first lines in the block are only performing
  * operations annotated with `@Parallelize` or typed as returning values
  * `in Pure`. The the parallelizable sub-blocks are analyzed separately,
  * possibly splitting their execution into separate threads, if allowed by the
  * dataflow dependencies.
  *
  * See inline documentation in the different methods of this pass for in-depth
  * explanations.
  */
object AutomaticParallelism extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default
  override val precursorPasses: Seq[IRPass] = Seq(
    AliasAnalysis,
    ComplexType
  )
  override val invalidatedPasses: Seq[IRPass] = Seq(
    AliasAnalysis,
    DataflowAnalysis
  )

  override def updateMetadataInDuplicate[T <: IR](sourceIr: T, copyOfIr: T): T =
    copyOfIr

  sealed private trait BlockAssignment
  private case object Top                extends BlockAssignment
  private case class ThreadOf(line: Int) extends BlockAssignment

  private case class Line(
    ir: IR.Expression,
    parallelismStatus: ParallelismStatus,
    id: Int,
    assignment: Option[AliasAnalysis.Graph.Id],
    dependencies: Set[Int],
    blockAssignment: Option[BlockAssignment]
  )

  private case class ParallelizationSegment(
    parallelizable: List[Line],
    pinned: List[IR.Expression]
  )

  private def assignDependencyClosure(
    segment: ParallelizationSegment
  ): ParallelizationSegment = {
    val withClosure = new Array[Line](segment.parallelizable.length)
    val newLines = segment.parallelizable.map { line =>
      withClosure.update(
        line.id,
        line.copy(dependencies =
          line.dependencies
            .flatMap(withClosure(_).dependencies)
            .union(line.dependencies)
        )
      )
      withClosure(line.id)
    }
    segment.copy(parallelizable = newLines)
  }

  private def assignBlocks(
    segment: ParallelizationSegment
  ): ParallelizationSegment = {
    val assignments =
      Array.fill[Option[BlockAssignment]](segment.parallelizable.length)(None)
    def moveTo(lineId: Int, block: BlockAssignment): Unit = {
      assignments(lineId) match {
        case None => assignments.update(lineId, Some(block))
        case Some(assignment) =>
          if (assignment != block) assignments.update(lineId, Some(Top))
      }
    }

    segment.parallelizable.reverse.foreach { line =>
      (line.parallelismStatus, assignments(line.id)) match {
        case (Parallelize, None) =>
          (Set(line.id) ++ line.dependencies)
            .foreach(moveTo(_, ThreadOf(line.id)))
        case _ =>
      }
    }

    segment.parallelizable.reverse.foreach { line =>
      assignments(line.id) match {
        case None =>
          val depAssignments: Set[BlockAssignment] =
            line.dependencies.flatMap(assignments).filter(_ != Top)
          val allMoved = Set(line.id) ++ line.dependencies
          depAssignments.toList match {
            case List(assignment) => allMoved.foreach(moveTo(_, assignment))
            case _                => allMoved.foreach(moveTo(_, Top))
          }
        case _ =>
      }
    }

    segment.copy(parallelizable =
      segment.parallelizable.zip(assignments).map { case (line, block) =>
        line.copy(blockAssignment = block)
      }
    )
  }

  @tailrec
  private def splitParallelBlocks(
    exprs: List[(IR.Expression, ParallelismStatus)],
    acc: List[ParallelizationSegment] = List()
  ): List[ParallelizationSegment] = exprs match {
    case Nil => acc.reverse
    case _ =>
      val (parallel, rest1) = exprs.span(_._2 != Pinned)
      val (pinned, rest)    = rest1.span(_._2 == Pinned)
      val parallelLines = parallel.zipWithIndex.map {
        case ((expr, status), ix) => Line(expr, status, ix, None, Set(), None)
      }
      val pinnedLines = pinned.map(_._1)
      splitParallelBlocks(
        rest,
        ParallelizationSegment(parallelLines, pinnedLines) :: acc
      )
  }

  private def fillAssignmentInfo(
    segment: ParallelizationSegment
  ): ParallelizationSegment =
    segment.copy(parallelizable = segment.parallelizable.map { line =>
      line.ir match {
        case bind: IR.Expression.Binding =>
          val aaInfo = bind
            .unsafeGetMetadata(
              AliasAnalysis,
              "Alias analysis left a binding behind"
            )
            .asInstanceOf[AliasAnalysis.Info.Occurrence]
          line.copy(assignment = Some(aaInfo.id))
        case _ => line
      }
    })

  private def gatherDeps(
    segment: ParallelizationSegment
  ): ParallelizationSegment = {
    val depMap = Map(segment.parallelizable.flatMap { line =>
      line.assignment.map { _ -> line.id }
    }: _*)
    val linesWithDeps = segment.parallelizable.map { line =>
      val deps = line.ir.preorder
        .collect { case n: IR.Name.Literal =>
          n
        }
        .flatMap(_.getMetadata(AliasAnalysis))
        .collect { case occ: AliasAnalysis.Info.Occurrence =>
          occ
        }
        .flatMap(occ => occ.graph.defLinkFor(occ.id))
        .flatMap(link => depMap.get(link.target))
        .filter(_ != line.id)
      line.copy(dependencies = Set(deps: _*))
    }
    segment.copy(parallelizable = linesWithDeps)
  }

  private def codeGen(
    segment: ParallelizationSegment,
    freshNameSupply: FreshNameSupply
  ): List[IR.Expression] = {
    val spawnedThreads = segment.parallelizable.groupBy(
      _.blockAssignment.getOrElse(
        throw new CompilerError("unassigned block in auto parallelism")
      )
    )
    val topExprs     = spawnedThreads.getOrElse(Top, List()).map(_.ir)
    val threadBlocks = spawnedThreads.removed(Top)
    if (threadBlocks.size < 2) {
      return segment.parallelizable.map(_.ir) ++ segment.pinned
    }

    val refVars = threadBlocks.values.flatten
      .map(_.ir)
      .collect { case bind: IR.Expression.Binding =>
        bind.name -> freshNameSupply.newName()
      }
      .toMap

    val refAllocations = refVars.values.map(
      IR.Expression
        .Binding(
          _,
          IR.Application.Prefix(
            IR.Name.Special(IR.Name.Special.NewRef, None),
            List(),
            false,
            None
          ),
          None
        )
        .updateMetadata(IgnoredBindings -->> IgnoredBindings.State.Ignored)
    )

    val threadSpawns = threadBlocks.values.map { exprs =>
      val blockBody =
        exprs.map(_.ir).flatMap {
          case bind: IR.Expression.Binding =>
            val refWrite = IR.Application.Prefix(
              IR.Name.Special(IR.Name.Special.WriteRef, None),
              List(
                IR.CallArgument
                  .Specified(None, refVars(bind.name).duplicate(), None),
                IR.CallArgument.Specified(None, bind.name.duplicate(), None)
              ),
              false,
              None
            )
            List(bind, refWrite)
          case other => List(other)
        }
      val spawn = IR.Application.Prefix(
        IR.Name.Special(IR.Name.Special.RunThread, None),
        List(
          IR.CallArgument.Specified(
            None,
            IR.Expression.Block(blockBody.init, blockBody.last, None),
            None
          )
        ),
        false,
        None
      )
      IR.Expression
        .Binding(freshNameSupply.newName(), spawn, None)
        .updateMetadata(IgnoredBindings -->> IgnoredBindings.State.Ignored)
    }

    val threadJoins = threadSpawns.map { bind =>
      IR.Application.Prefix(
        IR.Name.Special(IR.Name.Special.JoinThread, None),
        List(IR.CallArgument.Specified(None, bind.name.duplicate(), None)),
        false,
        None
      )
    }

    val varReads = refVars.map { case (name, ref) =>
      IR.Expression
        .Binding(
          name.duplicate(),
          IR.Application.Prefix(
            IR.Name.Special(IR.Name.Special.ReadRef, None),
            List(IR.CallArgument.Specified(None, ref.duplicate(), None)),
            false,
            None
          ),
          None
        )
        .updateMetadata(IgnoredBindings -->> IgnoredBindings.State.Ignored)
    }

    List(
      topExprs,
      refAllocations,
      threadSpawns,
      threadJoins,
      varReads,
      segment.pinned
    ).flatten
  }

  /** Executes the pass on a module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    @unused moduleContext: ModuleContext
  ): IR.Module = {
    val newBindings = ir.bindings.map {
      case method: IR.Module.Scope.Definition.Method.Explicit =>
        val newBody = withBodyBlock(method.body) { block =>
          val allExprs = block.expressions :+ block.returnValue
          val withParallelismStatus = allExprs.map { expr =>
            (expr, getParallelismStatus(expr))
          }
          val blocks          = splitParallelBlocks(withParallelismStatus)
          val withAssignments = blocks.map(fillAssignmentInfo)
          val withDeps =
            withAssignments.map(gatherDeps).map(assignDependencyClosure)
          val withBlocks = withDeps.map(assignBlocks)
//          if (method.methodName.name == "main") {
//            withBlocks.foreach { block =>
//              println("BLOCK START")
//              println("\tPARALLEL")
//              block.parallelizable.foreach { line =>
//                println(
//                  s"\t\t${line.id}\t${line.blockAssignment.get.toString
//                    .padTo(20, ' ')}${line.assignment.toString
//                    .padTo(10, ' ')}${line.parallelismStatus.toString
//                    .padTo(15, ' ')}${line.ir
//                    .showCode()
//                    .padTo(30, ' ')}${line.dependencies}"
//                )
//              }
//              println("\tPINNED")
//              block.pinned.foreach { line =>
//                println(s"\t\t${line.showCode()}")
//              }
//              println("END BLOCK")
//            }
//          }

          val newExprs =
            withBlocks.flatMap(codeGen(_, moduleContext.freshNameSupply.get))
//          println(
//            IR.Expression.Block(newExprs.init, newExprs.last, None).showCode()
//          )
          val r =
            block.copy(expressions = newExprs.init, returnValue = newExprs.last)
//          if (method.methodName.name == "main") {
//            println(r.showCode())
//          }
          r
        }
        method.copy(body = newBody)
      case other => other
    }
    ir.copy(bindings = newBindings)
  }

  sealed private trait ParallelismStatus {
    def sequencedWith(other: => ParallelismStatus): ParallelismStatus
  }

  private case object Pure extends ParallelismStatus {
    override def sequencedWith(other: => ParallelismStatus): ParallelismStatus =
      other
  }

  private case object Parallelize extends ParallelismStatus {
    override def sequencedWith(other: => ParallelismStatus): ParallelismStatus =
      other match {
        case Parallelize => Parallelize
        case Pure        => Parallelize
        case Pinned      => Pinned
      }
  }

  private case object Pinned extends ParallelismStatus {
    override def sequencedWith(other: => ParallelismStatus): ParallelismStatus =
      Pinned
  }

  @tailrec
  private def getMonad(signature: IR.Expression): Option[String] =
    signature match {
      case lam: IR.Function.Lambda => getMonad(lam.body)
      case app: IR.Application.Operator.Binary =>
        if (app.operator.name == "in") {
          app.right.value match {
            case lit: IR.Name.Literal => Some(lit.name)
            case _                    => None
          }
        } else if (app.operator.name == "->") { getMonad(app.right.value) }
        else None
      case _ => None
    }

  private def getParallelismStatus(expr: IR.Expression): ParallelismStatus =
    expr match {
      case app: IR.Application.Prefix =>
        app.function.getMetadata(MethodCalls) match {
          case Some(Resolution(method: ResolvedMethod)) =>
            val methodIr = method.getIr
            val isParallelize = methodIr
              .getMetadata(ModuleAnnotations)
              .exists(_.annotations.exists(_.name == "@Parallelize"))
            val monad = methodIr
              .getMetadata(TypeSignatures)
              .flatMap(sig => getMonad(sig.signature))
            val baseStatus: ParallelismStatus =
              if (isParallelize) Parallelize
              else if (monad.contains("Pure")) Pure
              else Pinned
//            if (method.method.name == "count_where") {
//              println("FOUND A COUNT WHERE " + monad + " " + baseStatus)
//            }
            app.arguments
              .map(_.value)
              .foldLeft(baseStatus)((status, ir) =>
                status.sequencedWith(getParallelismStatus(ir))
              )
          case _ => Pinned
        }
      case bind: IR.Expression.Binding => getParallelismStatus(bind.expression)
      case _: IR.Name                  => Pure
      case _: IR.Literal               => Pure
      case _: IR.Function.Lambda       => Pure
      case _                           => Pinned
    }

  private def withBodyBlock(
    expr: IR.Expression
  )(fn: IR.Expression.Block => IR.Expression.Block): IR.Expression =
    expr match {
      case fun: IR.Function.Binding =>
        fun.copy(body = withBodyBlock(fun.body)(fn))
      case fun: IR.Function.Lambda =>
        fun.copy(body = withBodyBlock(fun.body)(fn))
      case block: IR.Expression.Block if block.expressions.nonEmpty =>
        fn(block)
      case _ => expr
    }

  /** Executes the pass on an expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    *  @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    @unused inlineContext: InlineContext
  ): IR.Expression = ir

}
