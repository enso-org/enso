package org.enso.searcher.sql

import org.enso.searcher.Suggestion

import scala.util.Random

object SuggestionRandom {

  def nextKinds(): Seq[Suggestion.Kind] =
    Set.fill(1)(nextKind()).toSeq

  def nextSuggestion(): Suggestion = {
    nextKind() match {
      case Suggestion.Kind.Atom     => nextSuggestionAtom()
      case Suggestion.Kind.Method   => nextSuggestionMethod()
      case Suggestion.Kind.Function => nextSuggestionFunction()
      case Suggestion.Kind.Local    => nextSuggestionLocal()
    }
  }

  def nextSuggestionAtom(): Suggestion.Atom =
    Suggestion.Atom(
      name          = nextString(),
      arguments     = Seq(),
      returnType    = nextString(),
      documentation = optional(nextString())
    )

  def nextSuggestionMethod(): Suggestion.Method =
    Suggestion.Method(
      name          = nextString(),
      arguments     = Seq(),
      selfType      = nextString(),
      returnType    = nextString(),
      documentation = optional(nextString())
    )

  def nextSuggestionFunction(): Suggestion.Function =
    Suggestion.Function(
      name       = nextString(),
      arguments  = Seq(),
      returnType = nextString(),
      scope      = nextScope()
    )

  def nextSuggestionLocal(): Suggestion.Local =
    Suggestion.Local(
      name       = nextString(),
      returnType = nextString(),
      scope      = nextScope()
    )

  def nextScope(): Suggestion.Scope =
    Suggestion.Scope(
      start = Random.nextInt(Int.MaxValue),
      end   = Random.nextInt(Int.MaxValue)
    )

  def nextKind(): Suggestion.Kind =
    Random.nextInt(4) match {
      case 0 => Suggestion.Kind.Atom
      case 1 => Suggestion.Kind.Method
      case 2 => Suggestion.Kind.Function
      case 3 => Suggestion.Kind.Local
      case x => throw new NoSuchElementException(s"nextKind: $x")
    }

  def nextString(): String =
    Random.nextString(Random.nextInt(26) + 5)

  def optional[A](f: => A): Option[A] =
    Option.when(Random.nextBoolean())(f)
}
