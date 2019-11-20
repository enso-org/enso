package org.enso.compiler.core

import org.enso.compiler.core.IR.Literal.Text
import org.enso.syntax.text.AST
import org.enso.syntax.text.ast.text.Escape

// TODO [AA] REMOVE THIS ENTIRE FILE ONCE WE HAVE NEW CORE
/**
  * This is the compiler's high-level intermediate representation.
  *
  * [[IR]] is a close match for the program structure of the source language,
  * allowing it to be used for a number of high-level operations, including
  * desugaring and analysis passes that rely on the structure of the source
  * program to operate.
  */
sealed trait IR
object IR {

  /**
    * An expression is any language construct that returns a value, even if that
    * value is `Unit`.
    */
  sealed trait Expression extends IR

  /**
    * A module is the top-level construct of an Enso program.
    *
    * Modules currently have a one-to-one correspondence with the file scope,
    * but this design may change in the future.
    *
    * @param elements all constructs contained within the module
    */
  final case class Module(elements: List[IR]) extends Expression

  /**
    * An identifier is a name given to an Enso language construct.
    *
    * Each kind of identifier has different rules as to what constitutes
    * validity, but these rules are enforced by the parser and so need not be
    * checked at IR construction time.
    */
  sealed trait Identifier extends Expression
  object Identifier {
    final case class Blank()                   extends Identifier
    final case class Variable(name: String)    extends Identifier
    final case class Constructor(name: String) extends Identifier
    final case class Operator(name: String)    extends Identifier
    final case class Module(name: String)      extends Identifier
  }

  /**
    * A binding is any top-level construct that creates a source-level primitive
    * entity.
    */
  sealed trait Binding extends Expression
  object Binding {
    final case class Import(modulePath: List[Identifier.Constructor])
        extends Binding
    final case class Type() extends Binding
    final case class RawType(
      constructor: Identifier.Constructor,
      arguments: List[IR],
      body: Option[IR]
    ) extends Binding
    final case class Function()   extends Binding
    final case class Lambda()     extends Binding
    final case class Assignment() extends Binding
  }

  /**
    * An application is any IR entity that applies a function to zero or more
    * arguments.
    */
  sealed trait Application extends Expression
  object Application {
    final case class Prefix(fn: IR, arg: IR) extends Application
    final case class Infix(
      left: IR,
      fn: Identifier.Operator,
      right: IR
    ) extends Application
    final case class Mixfix(
      segments: List[Identifier],
      args: List[IR]
    ) extends Application

    /**
      * Operator sections are a syntactic construct that provide a short-hand
      * for partial application of operators.
      */
    sealed trait Section extends Application
    object Section {
      final case class Left(arg: IR, operator: Identifier.Operator)
          extends Section
      final case class Right(operator: Identifier.Operator, arg: IR)
          extends Section
      final case class Sides(operator: Identifier.Operator) extends Section
    }
  }

  /** Literals are constant values provided as part of the program's source. */
  sealed trait Literal extends Expression
  object Literal {
    final case class Number(number: String, base: Option[String])
        extends Literal

    /**
      * Text literals in Enso come in two main types.
      *
      * Raw text literals are uninterpolated, and are passed through as they are
      * provided in the program's source.
      *
      * Format text literals are literals that can contain Enso source-code
      * expressions spliced into the literal. These expressions can be as simple
      * as variable references, but are allowed to be arbitrary programs.
      */
    sealed trait Text extends Literal
    object Text {
      final case class Raw(body: List[Text.Line])    extends Text
      final case class Format(body: List[Text.Line]) extends Text

      final case class Line(lineSegments: List[Segment])

      sealed trait Segment extends Text
      object Segment {
        final case class Plain(text: String)          extends Segment
        final case class Expression(expr: Option[IR]) extends Segment
        final case class EscapeCode(escape: Escape)   extends Segment
      }
    }
  }

  /**
    * Control flow constructs allow encoding non-linear programs.
    *
    * Enso technically only has the `case ... of` statement as its sole control
    * flow construct. However, performance reasons force us to encode `if then`
    * and `if then else` as independent constructs rather than as part of the
    * standard library, so they are represented here.
    */
  sealed trait Control extends Expression
  object Control {
    final case class Case()       extends Expression
    final case class IfThen()     extends Expression
    final case class IfThenElse() extends Expression
  }

  /** Constructs that purely represent program structure. */
  sealed trait Block extends Expression
  object Block {
    final case class Enso(lines: List[IR])                         extends Block
    final case class Foreign(language: String, code: List[String]) extends Block
  }

  /** Constructs that represent various kinds of invalid programs. */
  sealed trait Error extends IR
  object Error {
    final case class UnexpectedToken(msg: String, unexpectedIR: List[IR])
        extends Error
    final case class UnrecognisedSymbol(symbol: String)            extends Error
    final case class EmptyGroup()                                  extends Error
    final case class UnhandledAST(ast: AST)                        extends Error
    final case class InvalidSuffix(identifier: IR, suffix: String) extends Error
    final case class UnclosedText(lines: List[Text.Line])          extends Error
  }

  /** Comments in the program source. */
  final case class Comment(lines: List[String]) extends IR

  /** A representation of type signatures */
  final case class Signature() extends IR

  // UTILITY FUNCTIONS ========================================================

  /**
    * Checks whether a given IR node represents an invalid portion of a program.
    *
    * @param ir the node to analyse
    * @return `true` if `ir` represents an invalid portion of the program,
    *        otherwise `false`
    */
  def isErrorNode(ir: IR): Boolean = ir match {
    case _: Error => true
  }

}
