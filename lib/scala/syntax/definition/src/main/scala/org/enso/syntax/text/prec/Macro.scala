package org.enso.syntax.text.prec

import org.enso.Logger
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.ast.meta.Builder
import org.enso.syntax.text.ast.meta.Builtin

import scala.annotation.{tailrec, unused}

object Macro {
  val logger = new Logger()

  //////////////////
  //// Registry ////
  //////////////////

  def run(module: AST.Module): AST.Module =
    module.map(transform)

  private def transform(t: AST): AST =
    new Transformer(t).run(AST.tokenize(t).toList())

  final private class Transformer(@unused t: AST) {
    val root: Builder.Context = Builder.Context(Builtin.registry.tree)

    var builder: Builder            = Builder.moduleBuilder()
    var builderStack: List[Builder] = Nil
    var isLineBegin: Boolean        = true

    def pushBuilder(name: AST.Ident, off: Int, lineBegin: Boolean): Unit =
      logger.trace {
        builderStack +:= builder
        builder = new Builder(name, off, lineBegin)
      }

    def popBuilder(): Option[Builder] = logger.trace {
      builderStack match {
        case Nil => None
        case b :: bs =>
          val out = builder
          builder      = b
          builderStack = bs
          Some(out)
      }
    }

    @tailrec
    def finish(): AST = {
      popBuilder() match {
        case Some(bldr) =>
          logger.log("End of input (in stack)")
          builder.merge(bldr)
          finish()
        case None =>
          logger.log("End of input (not in stack)")
          builder.buildAsModule()
      }
    }

    @tailrec
    def run(input: AST.Stream): AST = {
      input match {
        case Nil =>
          val builders                      = builder :: builderStack
          var newRevBuilders: List[Builder] = List()
          var subStream: AST.Stream         = List()
          for (bldr <- builders) {
            val noLastPattern = bldr.macroDef.map(_.last.pattern).contains(None)
            if (noLastPattern) {
              val (revLeftUnusedStream, matched, rightUnusedStream) =
                bldr.build(List())
              subStream =
                subStream ++ (rightUnusedStream.reverse :+ matched) ++ revLeftUnusedStream
            } else {
              bldr.current.revStream = subStream ++ bldr.current.revStream
              subStream              = List()
              newRevBuilders +:= bldr
            }

          }
          val newBuilders = newRevBuilders.reverse

          builder      = newBuilders.head
          builderStack = newBuilders.tail
          finish()
        case (t1 @ Shifted(off, AST.Ident.any(el1))) :: t2_ =>
          logger.log(s"Token $t1")
          logger.beginGroup()
          val wasLineBegin = isLineBegin
          isLineBegin = false
          builder.context.lookup(el1) match {
            case Some(tr) =>
              logger.log("New segment")
              builder.beginSegment(el1, off)
              builder.macroDef =
                tr.value.map(Some(_)).getOrElse(builder.macroDef)
              builder.context = builder.context.copy(tree = tr)
              logger.endGroup()
              run(t2_)

            case None =>
              root.lookup(el1) match {
                case Some(tr) =>
                  logger.log("New macro")
                  val context = builder.context
                  pushBuilder(el1, t1.off, wasLineBegin)
                  builder.macroDef = tr.value
                  builder.context  = Builder.Context(tr, Some(context))
                  logger.endGroup()
                  run(t2_)

                case _ =>
                  val currentClosed = builder.context.isEmpty
                  val parentPrecWin = (builder.current.ast, el1) match {
                    case (AST.Opr.any(_), _) => false
                    case (_, AST.Opr.any(_)) => true
                    case _                   => false
                  }
                  val parentBreak = builder.context.parentLookup(el1)
                  (currentClosed || parentPrecWin) && parentBreak match {
                    case true =>
                      logger.log("Parent close")
                      val subBuilder = builder
                      popBuilder()
                      builder.merge(subBuilder)
                      logger.endGroup()
                      run(input)
                    case false =>
                      logger.log("Add token")
                      builder.current.revStream +:= t1
                      logger.endGroup()
                      run(t2_)
                  }
              }
          }
        case Shifted(off, AST.Block.any(el1)) :: t2_ =>
          val nt1 = Shifted(off, el1.map(transform))
          builder.current.revStream +:= nt1
          run(t2_)

        case t1 :: t2_ =>
          builder.current.revStream +:= t1
          run(t2_)

      }
    }
  }
}
