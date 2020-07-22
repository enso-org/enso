package org.enso.flexer

import org.enso.Logger
import debug.Escape
import spec.Macro
import ReaderUTF.ENDOFINPUT

import scala.collection.mutable

trait Parser[T] {
  import Parser._

  var reader: Reader = _

  var status       = State.Status.Exit.OK
  val stateDefs    = new Array[Int => Int](256)
  val logger       = new Logger()
  var currentMatch = ""

  def getResult(): Option[T]

  def run(input: Reader): Result[T] = {
    reader = input
    reader.rewind.matched.set()
    reader.nextChar()

    while (state.runCurrent() == State.Status.Exit.OK) ()

    val value: Result.Value[T] = getResult() match {
      case None => Result.Failure(None)
      case Some(result) =>
        status match {
          case State.Status.Exit.FINISHED => Result.Success(result)
          case State.Status.Exit.FAIL     => Result.Failure(Some(result))
          case _                          => Result.Partial(result)
        }
    }
    Result(reader.offset, value)
  }

  // Rewind is a mechanism that allows for arbitrary lookahead.
  final def rewind(): Unit =
    reader.rewind.matched.run()

  //// State management ////

  // FIXME: This is a hack. Without it sbt crashes and needs to be completely
  //        cleaned to compile again.
  val state = _state
  final object _state {

    var registry = new mutable.ArrayBuffer[State]()

    def define(label: String = "unnamed", finish: => Unit = {}): State = {
      val groupIndex = registry.length
      val newState   = new State(label, groupIndex, () => finish)
      registry.append(newState)
      newState
    }

    var stack: List[State] = Nil
    var current: State     = define("Root")

    def begin(state: State): Unit = {
      logger.log(s"Begin ${state.label}")
      stack +:= current
      current = state
    }

    def end(): Unit = stack match {
      case Nil => logger.err("Trying to end root state")
      case head :: tail =>
        logger.log(s"End ${current.label}, back to ${head.label}")
        current = head
        stack   = tail
    }

    def endTill(s: State): Unit = logger.trace {
      while (s != state.current) {
        state.current.finish()
        state.end()
      }
    }

    def isInside(state: State): Boolean =
      current == state || stack.contains(state)

    def runCurrent(): Int = {
      val cstate    = state.current
      var finished  = false
      val nextState = stateDefs(cstate.ix)
      status = State.Status.INITIAL
      while (State.valid(status)) {
        logger.log(
          s"Step (${cstate.ix}:$status) "
          + s"${Escape.str(reader.currentStr)} (${reader.charCode})"
        )
        status = nextState(status)
        if (finished && !reader.rewind.rewinded)
          status = State.Status.Exit.FINISHED
        finished = reader.charCode == ENDOFINPUT
        if (State.valid(status)) {
          if (reader.charCode != ENDOFINPUT)
            reader.result.appendCodePoint(reader.charCode)
          reader.nextChar()
        }
      }
      status
    }

    def call(rule: () => Unit): State.Status.Exit = {
      currentMatch = reader.result.toString
      rule()
      reader.result.setLength(0)
      reader.rewind.matched.set()
      State.Status.Exit.OK
    }

  }

  val ROOT = state.current
}

object Parser {

  val BUFFER_SIZE   = 16384
  val UTF_CHAR_SIZE = 2

  val eofCodePoint: Int = -1
  val etxCodePoint: Int = -2

  object State {
    object Status {
      val INITIAL = 0
      type Exit = Int
      object Exit {
        val OK       = -1
        val FAIL     = -2
        val FINISHED = -3
      }
    }
    def valid(i: Int): Boolean =
      i >= 0
  }

  def compile[T, P](p: P)(implicit ev: P <:< Parser[T]): () => P =
    macro Macro.compileImpl[T, P]

  case class Result[T](offset: Int, value: Result.Value[T]) {
    def map[S](fn: T => S): Result[S] = copy(value = value.map(fn))
  }
  object Result {
    sealed trait Value[T] {
      def map[S](fn: T => S): Value[S]
    }
    final case class Success[T](result: T) extends Value[T] {
      def map[S](fn: T => S) = copy(fn(result))
    }
    final case class Partial[T](result: T) extends Value[T] {
      def map[S](fn: T => S) = copy(fn(result))
    }
    final case class Failure[T](result: Option[T]) extends Value[T] {
      def map[S](fn: T => S) = copy(result.map(fn))
    }
  }

}
