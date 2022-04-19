package org.enso.compiler.codegen

import org.enso.data
import org.enso.data.List1
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Ident.{Opr, Var}

/** This object contains view patterns that allow matching on the parser [[AST]]
  * for more sophisticated constructs.
  *
  * These view patterns are implemented as custom unapply methods that only
  * return [[Some]] when more complex conditions are met. These view patterns
  * return the [[AST]] representations of the relevant segments in order to
  * allow location information to easily be provided to the translation
  * mechanism.
  */
object AstView {

  object Atom {

    /** Matches a primitive atom definition only.
      *
      * It will _not_ match a type definition with a body.
      *
      * @param ast the structure to try and match on
      * @return the name of the atom, and the arguments to the atom
      */
    def unapply(ast: AST): Option[(AST.Ident, List[AST])] =
      ast match {
        case AST.Def(name, args, body) if body.isEmpty => Some((name, args))
        case _                                         => None
      }
  }

  object TypeDef {

    /** Matches a complex type definition.
      *
      * @param ast the structure to try and match on
      * @return the name of the type, the arguments to it, and the type body
      */
    def unapply(ast: AST): Option[(AST.Ident, List[AST], AST)] =
      ast match {
        case AST.Def(name, args, body) if body.isDefined =>
          Some((name, args, body.get))
        case _ => None
      }
  }

  object Block {

    /** Matches an arbitrary block in the program source.
      *
      * @param ast the structure to try and match on
      * @return a list of expressions in the block, and the final expression
      *         separately
      */
    def unapply(ast: AST): Option[(List[AST], AST)] =
      ast match {
        case AST.Block(_, _, firstLine, lines) =>
          val actualLines = firstLine.elem :: lines.flatMap(_.elem)
          if (actualLines.nonEmpty) {
            Some((actualLines.dropRight(1), actualLines.last))
          } else {
            None
          }
        case _ => None
      }
  }

  object SuspendedBlock {

    /** Matches an arbitrary suspended block in the program source.
      *
      * A suspended block is one that is bound to a name but takes no arguments.
      *
      * @param ast the structure to try and match on
      * @return the name to which the block is assigned, and the block itself
      */
    def unapply(ast: AST): Option[(AST.Ident, AST.Block)] = {
      ast match {
        case BasicAssignment(name, AST.Block.any(block)) =>
          Some((name, block))
        case _ => None
      }
    }
  }

  object Binding {
    val bindingOpSym: Opr = AST.Ident.Opr("=")

    /** Matches an arbitrary binding in the program source.
      *
      * A binding is any expression of the form `<expr> = <expr>`, and this
      * matcher asserts no additional properties on the structure it matches.
      *
      * @param ast the structure to try and match on
      * @return the expression on the left of the binding operator, and the
      *         expression on the right side of the binding operator
      */
    def unapply(ast: AST): Option[(AST, AST)] = {
      ast match {
        case AST.App.Infix.any(ast) =>
          val left  = ast.larg
          val op    = ast.opr
          val right = ast.rarg

          if (op == bindingOpSym) {
            Some((left, right))
          } else {
            None
          }
        case _ => None
      }
    }
  }

  object BasicAssignment {
    val assignmentOpSym: Opr = AST.Ident.Opr("=")

    /** Matches an assignment.
      *
      * An assignment is a [[Binding]] where the left-hand side is a variable
      * name.
      *
      * @param ast the structure to try and match on
      * @return the variable name assigned to, and the expression being assigned
      */
    def unapply(ast: AST): Option[(AST.Ident, AST)] = {
      ast match {
        case Binding(AST.Ident.Var.any(left), right)   => Some((left, right))
        case Binding(AST.Ident.Blank.any(left), right) => Some((left, right))
        case _                                         => None
      }
    }
  }

  object MaybeBlankName {
    val blankSym: String = "_"

    /** Matches an identifier that may be a blank `_`.
      *
      * @param ast the structure to try and match on
      * @return the identifier
      */
    def unapply(ast: AST): Option[AST.Ident] = {
      ast match {
        case AST.Ident.Var.any(variable) => Some(variable)
        case AST.Ident.Cons.any(cons)    => Some(cons)
        case AST.Ident.Blank.any(blank)  => Some(blank)
        case _                           => None
      }
    }
  }

  object Lambda {
    val lambdaOpSym: Opr = AST.Ident.Opr("->")

    /** Matches a lambda expression in the program source.
      *
      * A lambda expression is of the form `<args> -> <expression>` where
      * `<args>` is a space-separated list of valid argument definitions, and
      * `<expression>` is an arbitrary program expression.
      *
      * @param ast the structure to try and match on
      * @return a list of the arguments defined for the lambda, and the body of
      *         the lambda
      */
    def unapply(ast: AST): Option[(List[AST], AST)] = {
      ast match {
        case AST.App.Infix.any(ast) =>
          val left  = ast.larg
          val op    = ast.opr
          val right = ast.rarg

          if (op == lambdaOpSym) {
            left match {
              case FunctionParamList(args) => Some((args, right))
              case _                       => None
            }
          } else {
            None
          }
        case _ => None
      }
    }
  }

  object LazyArgument {

    /** Matches on a lazy argument definition or usage.
      *
      * A lazy argument is one of the form `~t` where `t` is a valid parameter
      * name. This is temporary syntax and will be removed once we have the
      * ability to insert these analyticallyl
      *
      * @param ast the structure to try and match on
      * @return the term being forced
      */
    def unapply(ast: AST): Option[AST] =
      ast match {
        case MaybeManyParensed(
              AST.App.Section.Right(AST.Ident.Opr("~"), FunctionParam(arg))
            ) =>
          Some(arg)
        case _ => None
      }
  }

  object FunctionParam {

    /** Matches a definition-site function parameter.
      *
      * @param ast the structure to try and match on
      * @return the parameter definition
      */
    def unapply(ast: AST): Option[AST] =
      ast match {
        case AscribedArgument(_, _, _, _)         => Some(ast)
        case LazyAssignedArgumentDefinition(_, _) => Some(ast)
        case AssignedArgument(_, _)               => Some(ast)
        case DefinitionArgument(_)                => Some(ast)
        case LazyArgument(_)                      => Some(ast)
        case _                                    => None
      }
  }

  object FunctionParamList {

    /** Matches on the parameter list of a lambda.
      *
      * @param ast the structure to try and match on
      * @return a list of the arguments for which the lambda is defined
      */
    def unapply(ast: AST): Option[List[AST]] = {
      ast match {
        case SpacedList(args) =>
          val realArgs = args.collect { case a @ FunctionParam(_) => a }

          if (realArgs.length == args.length) {
            Some(args)
          } else {
            None
          }
        case FunctionParam(p) => Some(List(p))
        case _                => None
      }
    }
  }

  object MaybeTyped {

    /** Matches on terms that _may_ have a type signature.
      *
      * Such terms take the form of `<term> : <type>`, where both `<term>` and
      * `<type>` can be arbitrary program expressions.
      *
      * @param ast the structure to try and match on
      * @return the term and the type ascribed to it
      */
    def unapply(ast: AST): Option[(AST, AST)] =
      ast match {
        case AST.App.Infix(entity, AST.Ident.Opr(":"), signature) =>
          Some((entity, signature))
        case _ => None
      }
  }

  object FunctionSugar {

    /** Matches on terms that represent long-form function definitions.
      *
      * @param ast the structure to try and match on
      * @return the name of the function, the list of function arguments, and
      *         the function body
      */
    def unapply(ast: AST): Option[(AST.Ident, List[AST], AST)] = {
      ast match {
        case AstView.Binding(left, body) =>
          left match {
            case FunctionParamList(items) =>
              if (items.length > 1) {
                val fnName = items.head
                val args   = items.tail

                fnName match {
                  case AST.Ident.Var.any(name) => Some((name, args, body))
                  case _                       => None
                }
              } else {
                None
              }
            case _ => None
          }
        case _ => None
      }
    }
  }

  object Parensed {

    /** Matches on terms that is surrounded by parentheses.
      *
      * It 'peels off' one layer of parentheses, so the returned term may still
      * be surrounded by more parentheses.
      *
      * @param ast the structure to try and match on
      * @return the term contained in the parentheses
      */
    def unapply(ast: AST): Option[AST] = {
      AST.Group.unapply(ast).flatten
    }
  }

  object MaybeParensed {

    /** Matches on terms that _may_ be surrounded by (an arbitrary amount of)
      * parentheses.
      *
      * It 'peels off' one layer of parentheses, so the returned term may still
      * be surrounded by more parentheses.
      *
      * @param ast the structure to try and match on
      * @return the term contained in the parentheses
      */
    def unapply(ast: AST): Option[AST] = {
      ast match {
        case AST.Group(mExpr) => mExpr
        case a                => Some(a)
      }
    }
  }

  object ManyParensed {

    /** Matches on terms that is surrounded by an arbitrary (but non-zero)
      * amount of parentheses.
      *
      * The resulting term is not surrounded by any more parentheses.
      *
      * @param ast the structure to try and match on
      * @return the term contained in the parentheses
      */
    def unapply(ast: AST): Option[AST] = {
      ast match {
        case Parensed(MaybeManyParensed(p)) => Some(p)
        case _                              => None
      }
    }
  }

  object MaybeManyParensed {

    /** Matches on terms that _may_ be surrounded by (an arbitrary amount of)
      * parentheses.
      *
      * The resulting term is not surrounded by any more parentheses.
      *
      * @param ast the structure to try and match on
      * @return the term contained in the parentheses
      */
    def unapply(ast: AST): Option[AST] = {
      ast match {
        case AST.Group(mExpr) => mExpr.flatMap(unapply)
        case a                => Some(a)
      }
    }
  }

  object AssignedArgument {

    /** Matches on the structure of an 'assigned argument'.
      *
      * Such an argument has the structure `<var> = <expression>` where `<var>`
      * must be a valid variable name, and `<expression>` is an arbitrary Enso
      * expression.
      *
      * @param ast the structure to try and match on
      * @return the variable name and the expression being bound to it
      */
    def unapply(ast: AST): Option[(AST.Ident, AST)] =
      MaybeManyParensed.unapply(ast).flatMap(BasicAssignment.unapply)
  }

  object AscribedArgument {

    /** Matches on the structure of an 'ascribed argument'.
      *
      * Such an argument has the structure
      * `[~]<var> : <expression> [= <expression]`, where `<var>` must be a valid
      * variable name, and both `<expression>`s are arbitrary Enso expressions.
      * The `~` to mark laziness is optional.
      *
      * @param ast the structure to try and match on
      * @return the variable name, the type being ascribed to it, the value
      *         being bound to it, and whether or not it was marked as lazy
      */
    def unapply(ast: AST): Option[(AST.Ident, AST, Option[AST], Boolean)] = {
      MaybeManyParensed.unapply(ast).flatMap(matchBareArg)
    }

    def matchBareArg(
      ast: AST
    ): Option[(AST.Ident, AST, Option[AST], Boolean)] = {
      ast match {
        case AST.App.Infix(maybeVarAndType, AST.Ident.Opr("="), exprVal) =>
          maybeVarAndType match {
            case AST.App.Infix(varName, AST.Ident.Opr(":"), expr) =>
              varName match {
                case AST.Ident.Var.any(v) =>
                  Some((v, expr, Some(exprVal), false))
                case AST.Ident.Blank.any(v) =>
                  Some((v, expr, Some(exprVal), false))
                case AST.App.Section
                      .Right(AST.Ident.Opr("~"), AST.Ident.Var.any(v)) =>
                  Some((v, expr, Some(exprVal), true))
                case AST.App.Section
                      .Right(AST.Ident.Opr("~"), AST.Ident.Blank.any(v)) =>
                  Some((v, expr, Some(exprVal), true))
                case _ => None
              }
            case _ => None
          }
        case AST.App.Infix(varName, AST.Ident.Opr(":"), expr) =>
          varName match {
            case AST.Ident.Var.any(v) =>
              Some((v, expr, None, false))
            case AST.Ident.Blank.any(v) =>
              Some((v, expr, None, false))
            case AST.App.Section
                  .Right(AST.Ident.Opr("~"), AST.Ident.Var.any(v)) =>
              Some((v, expr, None, true))
            case AST.App.Section
                  .Right(AST.Ident.Opr("~"), AST.Ident.Blank.any(v)) =>
              Some((v, expr, None, true))
            case _ => None
          }
        case _ => None
      }
    }

  }

  object LazyAssignedArgumentDefinition {

    /** Matches on the definition of a lazy argument for a function that also
      * has a default value.
      *
      * @param ast the structure to try and match on
      * @return the name of the argument being declared and the expression of
      *         the default value being bound to it
      */
    def unapply(ast: AST): Option[(AST.Ident, AST)] = {
      ast match {
        case MaybeManyParensed(
              Binding(
                AST.App.Section.Right(AST.Ident.Opr("~"), AST.Ident.Var.any(v)),
                r
              )
            ) =>
          Some((v, r))
        case _ => None
      }
    }
  }

  object DefinitionArgument {

    /** Matches on a definition argument, which is a standard variable
      * identifier.
      *
      * @param ast the structure to try and match on
      * @return the name of the argument
      */
    def unapply(ast: AST): Option[AST.Ident] =
      ast match {
        case MaybeManyParensed(MaybeBlankName(ast)) => Some(ast)
        case _                                      => None
      }
  }

  /** A union type for application matchers. */
  sealed trait MethodOrExpr

  /** A wrapper for applications denoting the name as being a method name.
    * @param ast the original identifier
    */
  case class Method(ast: AST.Ident) extends MethodOrExpr

  /** A wrapper for applications denoting the function as not-a-method.
    * @param ast the original AST
    */
  case class Expr(ast: AST) extends MethodOrExpr

  object Application {

    /** Matches an arbitrary function application. This includes both method
      * calls and standard function applications as they are syntactically
      * unified.
      *
      * @param ast the structure to try and match on
      * @return the name of the function, and a list of its arguments (including
      *         the `self` argument if using method-call syntax)
      */
    def unapply(ast: AST): Option[(MethodOrExpr, List[AST])] =
      SpacedList.unapply(ast).flatMap {
        case fun :: args =>
          fun match {
            case MethodCall(target, function, methodArgs) =>
              Some((Method(function), target :: methodArgs ++ args))
            case _ => Some((Expr(fun), args))
          }
        case _ => None
      }
  }

  object MethodCall {

    private def consToVar(ast: AST.Ident): AST.Ident =
      ast match {
        case AST.Ident.Cons(c) =>
          AST.Ident.Var(c).setLocation(ast.location).setID(ast.id)
        case _ => ast
      }

    /** Matches on a method call.
      *
      * A method call has the form `<obj>.<fn-name> <args...>` where `<obj>` is
      * an arbitrary expression, `<fn-name>` is the name of the function being
      * called, and `<args>` are the arguments to the call.
      *
      * @param ast the structure to try and match on
      * @return the `self` expression, the function name, and the arguments to
      *         the function
      */
    def unapply(ast: AST): Option[(AST, AST.Ident, List[AST])] =
      ast match {
        case OperatorDot(target, Application(Expr(ConsOrVar(ident)), args)) =>
          Some((target, consToVar(ident), args))
        case AST.App.Section.Left(
              MethodCall(target, ident, List()),
              susp @ SuspendDefaultsOperator(_)
            ) =>
          Some((target, ident, List(susp)))
        case OperatorDot(target, ConsOrVar(ident)) =>
          Some((target, consToVar(ident), List()))
        case _ => None
      }
  }

  object SuspendDefaultsOperator {

    /** Matches on a usage of the `...` 'suspend defaults' operator.
      *
      * @param ast the structure to try and match on
      * @return the 'suspend defaults' operator
      */
    def unapply(ast: AST): Option[AST] = {
      ast match {
        case AST.Ident.Opr("...") => Some(ast)
        case _                    => None
      }
    }
  }

  object SpacedList {

    /** Matches an arbitrary space-separated list in the AST, possibly including
      * a usage of the `...` operator.
      *
      * @param ast the structure to try and match on
      * @return the elements of the list
      */
    def unapply(ast: AST): Option[List[AST]] = {
      matchSpacedList(ast)
    }

    private[this] def matchSpacedList(ast: AST): Option[List[AST]] = {
      ast match {
        case MaybeManyParensed(AST.App.Prefix(fn, arg)) =>
          val fnRecurse = matchSpacedList(fn)

          fnRecurse match {
            case Some(headItems) => Some(headItems :+ arg)
            case None            => Some(List(fn, arg))
          }
        case MaybeManyParensed(
              AST.App.Section.Left(ast, SuspendDefaultsOperator(suspend))
            ) =>
          ast match {
            case ConsOrVar(_) => Some(List(ast, suspend))
            case _ =>
              val astRecurse = matchSpacedList(ast)

              astRecurse match {
                case Some(items) => Some(items :+ suspend)
                case None        => None
              }
          }
        case _ => None
      }
    }
  }

  object MethodDefinition {

    /** Matches on the definition of a method.
      *
      * These take the form of `<type>.<fn-name> = <expression>`,
      * or `<fn-name> = <expression>`, where `<type>` is the name of a type,
      * `<fn-name>` is the name of a function, and `<expression>` is an
      * arbitrary program expression.
      *
      * @param ast the structure to try and match on
      * @return the path segments of the type reference, the function name, the
      *         arguments to the method, and the bound expression
      */
    def unapply(ast: AST): Option[(List[AST], AST, List[AST], AST)] = {
      ast match {
        case Binding(lhs, rhs) =>
          lhs match {
            case MethodReference(targetPath, name) =>
              Some((targetPath, name, List(), rhs))
            case MethodBindingLHS(path, methodName, args) =>
              Some((path, methodName, args, rhs))
            case AST.App.Section.Right(opr, arg) =>
              Some((List(), opr, List(arg), rhs))
            case AST.Ident.Var.any(name) => Some((List(), name, List(), rhs))
            case _                       => None
          }
        case _ => None
      }
    }
  }

  object MethodBindingLHS {

    /** Matches on the left hand side of a sugared method definition.
      *
      * @param ast the structure to try and match on
      * @return the path segments of the type reference, the function name, and
      *         the arguments
      */
    def unapply(ast: AST): Option[(List[AST], AST, List[AST])] = {
      ast match {
        case SpacedList(MethodReference(path, methodName) :: args) =>
          val validArgs = args.forall {
            case AstView.FunctionParam(_) => true
            case _                        => false
          }

          if (validArgs) Some((path, methodName, args)) else None
        case _ => None
      }
    }
  }

  object ConsOrVar {

    /** Matches any expression that is either the name of a constructor or a
      * variable.
      *
      * @param arg the structure to try and match on
      * @return the identifier matched on
      */
    def unapply(arg: AST): Option[AST.Ident] =
      arg match {
        case AST.Ident.Var.any(arg)  => Some(arg)
        case AST.Ident.Cons.any(arg) => Some(arg)
        case _                       => None
      }
  }

  object OperatorDot {

    /** Matches on an arbitrary usage of operator `.` with no restrictions on
      * the operands.
      *
      * @param ast the structure to try and match on
      * @return the left- and right-hand sides of the operator
      */
    def unapply(ast: AST): Option[(AST, AST)] =
      ast match {
        case AST.App.Infix(left, AST.Ident.Opr("."), right) =>
          Some((left, right))
        case _ =>
          None
      }
  }

  object DotChain {

    /** Matches an arbitrary chain of [[OperatorDot]] expressions.
      *
      * @param ast the structure to try and match on
      * @return the segments making up the chain
      */
    def unapply(ast: AST): Option[List[AST]] = {
      val path = matchDotChain(ast)

      if (path.isEmpty) {
        None
      } else {
        Some(path)
      }
    }

    private[this] def matchDotChain(ast: AST): List[AST] = {
      ast match {
        case OperatorDot(left, right) => matchDotChain(left) :+ right
        case AST.Ident.any(ast)       => List(ast)
        case _                        => List()
      }
    }
  }

  object MethodReference {

    /** Matches on a method reference.
      *
      * A method reference is a [[DotChain]] where all but the last element are
      * the names of constructors.
      *
      * @param ast the structure to try and match on
      * @return the constructor segments and the final segment
      */
    def unapply(ast: AST): Option[(List[AST], AST)] = {
      ast match {
        case DotChain(segments) =>
          if (segments.length >= 2) {
            val consPath = segments.dropRight(1)
            val maybeVar = segments.last

            val isValid = consPath.collect { case a @ AST.Ident.Cons(_) =>
              a
            }.length == consPath.length

            if (isValid) {
              maybeVar match {
                case AST.Ident.Var(_) => Some((consPath, maybeVar))
                case _                => None
              }
            } else {
              None
            }
          } else {
            None
          }
        case _ => None
      }
    }
  }

  object CaseExpression {
    val caseName: List1[Var] =
      data.List1(AST.Ident.Var("case"), AST.Ident.Var("of"))

    /** Matches on a case expression.
      *
      * A case expression is of the following form:
      *
      * {{{
      *   case <scrutinee> of
      *     ## <comment>
      *     <matcher> -> <expression>
      *     <...>
      * }}}
      *
      * where:
      * - `<scrutinee>` is an arbitrary non-block program expression
      * - `<matcher>` is a [[Pattern]]
      * - `<expression>` is an arbirary program expression
      * - `<comment>` is an optional documentation comment
      *
      * @param ast the structure to try and match on
      * @return the scrutinee and a list of the case branches and comments
      */
    def unapply(ast: AST): Option[(AST, List[AST])] = {
      ast match {
        case AST.Mixfix(identSegments, argSegments) =>
          if (identSegments == caseName) {
            if (argSegments.length == 2) {
              val scrutinee    = argSegments.head
              val caseBranches = argSegments.last

              caseBranches match {
                case AST.Block(_, _, firstLine, restLines) =>
                  val blockLines = firstLine.elem :: restLines.flatMap(_.elem)

                  val matchBranches = blockLines.collect {
                    case b @ CaseBranch(_, _) => b
                    case c @ AST.Comment(_)   => c
                  }

                  if (matchBranches.length == blockLines.length) {
                    Some((scrutinee, matchBranches))
                  } else {
                    None
                  }
                case _ => None
              }
            } else {
              None
            }
          } else {
            None
          }
        case _ => None
      }
    }
  }

  object CaseBranch {

    /** Matches on an arbitrary pattern match case branch.
      *
      * A case branch has the form `<matcher> -> <expression>`, where
      * `<matcher>` is an expression that can match on the scrutinee, and
      * `<expression>` is an arbitrary expression to execute on a successful
      * match.
      *
      * @param ast the structure to try and match on
      * @return the pattern, and the branch expression
      */
    def unapply(ast: AST): Option[(AST, AST)] = {
      ast match {
        case AST.App.Infix(left, AST.Ident.Opr("->"), right) =>
          left match {
            case Pattern(pat) => Some((pat, right))
            case _            => None
          }
        case _ => None
      }
    }
  }

  object Pattern {

    /** Matches on an arbitrary pattern expression.
      *
      * @param ast the structure to try and match on
      * @return the pattern
      */
    def unapply(ast: AST): Option[AST] = {
      ast match {
        case ConstructorPattern(_, _) => Some(ast)
        case CatchAllPattern(pat)     => Some(pat)
        case Parensed(Pattern(p)) =>
          println(p)
          Some(p)
        case _ => None
      }
    }
  }

  object QualifiedName {
    def unapply(ast: AST): Option[List[AST.Ident.Cons]] =
      ast match {
        case OperatorDot(l, AST.Ident.Cons.any(name)) =>
          unapply(l).map(_ :+ name)
        case AST.Ident.Cons.any(name) =>
          Some(List(name))
        case _ => None
      }
  }

  object ConstructorPattern {

    /** Matches on a constructor pattern.
      *
      * Constructor patterns take the form `<ref-name> <fields...>`, where
      * `<ref-name>` is a referent name, and `<fields>` is a maybe empty list of
      * patterns.
      *
      * @param ast the structure to try and match on
      * @return the pattern
      */
    def unapply(ast: AST): Option[(List[AST.Ident.Cons], List[AST])] = {
      MaybeManyParensed.unapply(ast).getOrElse(ast) match {
        case QualifiedName(cons) => Some((cons, List()))
        case SpacedList(elems) if elems.nonEmpty =>
          elems.head match {
            case QualifiedName(refName) =>
              val allFieldsValid = elems.tail.forall {
                case Pattern(_) => true
                case _          => false
              }

              if (allFieldsValid) {
                Some((refName, elems.tail))
              } else {
                None
              }
            case _ => None
          }
        case _ => None
      }
    }
  }

  object CatchAllPattern {

    /** Matches on a catch all pattern.
      *
      * A catch all pattern takes the form of a name, which may be blank.
      *
      * @param ast the structure to try and match on
      * @return the pattern
      */
    def unapply(ast: AST): Option[AST.Ident] = {
      MaybeManyParensed.unapply(ast).getOrElse(ast) match {
        case AST.Ident.any(ident) => Some(ident)
        case _                    => None
      }
    }
  }

  object DecimalLiteral {
    def unapply(ast: AST): Option[(AST.Literal.Number, AST.Literal.Number)] =
      ast match {
        case AST.App.Infix(
              AST.Literal.Number.any(int),
              AST.Ident.Opr("."),
              AST.Literal.Number.any(frac)
            ) =>
          Some((int, frac))
        case _ => None
      }
  }

  object UnaryMinus {
    def minusSymbol: String = "-"

    /** Matches a unary minus.
      *
      * It should be noted that this is a hack that matches a spaced section as
      * well for now. This will be improved with the new parser.
      *
      * @param ast the structure to try and match on
      * @return the negated expression
      */
    def unapply(ast: AST): Option[AST] =
      ast match {
        case MaybeManyParensed(
              AST.App.Section.Right(AST.Ident.Opr("-"), expression)
            ) =>
          Some(expression)
        case _ => None
      }
  }

  object TypeAscription {
    val operatorName: String = ":"

    /** Matches a usage of the type ascription operator `:`.
      *
      * @param ast the structure to try and match on
      * @return the typed expression, and the ascribed type
      */
    def unapply(ast: AST): Option[(AST, AST)] =
      ast match {
        case MaybeManyParensed(AST.App.Infix(typed, AST.Ident.Opr(op), sig))
            if op == operatorName =>
          Some((typed, sig))
        case _ => None
      }
  }
}
