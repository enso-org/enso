package org.enso.compiler.context

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.resolve.{
  DocumentationComments,
  MethodDefinitions,
  TypeSignatures
}
import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.Tree
import org.enso.syntax.text.Location
import org.enso.text.editing.IndexedSource

import scala.collection.mutable

/** Module that extracts [[Suggestion]] entries from the [[IR]].
  *
  * @param source the text source
  * @tparam A the type of the text source
  */
final class SuggestionBuilder[A: IndexedSource](val source: A) {

  import SuggestionBuilder._

  /** Build suggestions from the given `ir`.
    *
    * @param module the module name
    * @param ir the input `IR`
    * @return the tree of suggestion entries extracted from the given `IR`
    */
  def build(module: String, ir: IR): Tree.Root[Suggestion] = {
    type TreeBuilder =
      mutable.Builder[Tree.Node[Suggestion], Vector[Tree.Node[Suggestion]]]
    def go(tree: TreeBuilder, scope: Scope): Vector[Tree.Node[Suggestion]] = {
      if (scope.queue.isEmpty) {
        tree.result()
      } else {
        val ir  = scope.queue.dequeue()
        val doc = ir.getMetadata(DocumentationComments).map(_.documentation)
        ir match {
          case IR.Module.Scope.Definition.Atom(name, arguments, _, _, _) =>
            val suggestions = buildAtom(module, name.name, arguments, doc)
            go(tree ++= suggestions.map(Tree.Node(_, Vector())), scope)

          case IR.Module.Scope.Definition.Method
                .Explicit(
                  IR.Name.MethodReference(typePtr, methodName, _, _, _),
                  IR.Function.Lambda(args, body, _, _, _, _),
                  _,
                  _,
                  _
                ) =>
            val typeSignature = ir.getMetadata(TypeSignatures)
            val selfTypeOpt =
              typePtr.getMetadata(MethodDefinitions).flatMap(buildSelfType)
            val methodOpt = selfTypeOpt.map { selfType =>
              buildMethod(
                body.getExternalId,
                module,
                methodName,
                selfType,
                args,
                doc,
                typeSignature
              )
            }
            val subforest = go(
              Vector.newBuilder,
              Scope(body.children, body.location)
            )
            go(tree ++= methodOpt.map(Tree.Node(_, subforest)), scope)

          case IR.Expression.Binding(
                name,
                IR.Function.Lambda(args, body, _, _, _, _),
                _,
                _,
                _
              ) if name.location.isDefined =>
            val typeSignature = ir.getMetadata(TypeSignatures)
            val function = buildFunction(
              body.getExternalId,
              module,
              name,
              args,
              scope.location.get,
              typeSignature
            )
            val subforest = go(
              Vector.newBuilder,
              Scope(body.children, body.location)
            )
            go(tree += Tree.Node(function, subforest), scope)

          case IR.Expression.Binding(name, expr, _, _, _)
              if name.location.isDefined =>
            val typeSignature = ir.getMetadata(TypeSignatures)
            val local = buildLocal(
              expr.getExternalId,
              module,
              name.name,
              scope.location.get,
              typeSignature
            )
            val subforest = go(
              Vector.newBuilder,
              Scope(expr.children, expr.location)
            )
            go(tree += Tree.Node(local, subforest), scope)

          case _ =>
            go(tree, scope)
        }
      }
    }

    Tree.Root(
      go(Vector.newBuilder, Scope(ir.children, ir.location))
    )
  }

  /** Build a method suggestion. */
  private def buildMethod(
    externalId: Option[IR.ExternalId],
    module: String,
    name: IR.Name,
    selfType: String,
    args: Seq[IR.DefinitionArgument],
    doc: Option[String],
    typeSignature: Option[TypeSignatures.Metadata]
  ): Suggestion.Method = {
    typeSignature match {
      case Some(TypeSignatures.Signature(typeExpr)) =>
        val typeSig = buildTypeSignature(typeExpr)
        val (methodArgs, returnTypeDef) =
          buildMethodArguments(args, typeSig, selfType)
        Suggestion.Method(
          externalId    = externalId,
          module        = module,
          name          = name.name,
          arguments     = methodArgs,
          selfType      = selfType,
          returnType    = buildReturnType(returnTypeDef),
          documentation = doc
        )
      case _ =>
        Suggestion.Method(
          externalId    = externalId,
          module        = module,
          name          = name.name,
          arguments     = args.map(buildArgument),
          selfType      = selfType,
          returnType    = Any,
          documentation = doc
        )
    }
  }

  /** Build a function suggestion */
  private def buildFunction(
    externalId: Option[IR.ExternalId],
    module: String,
    name: IR.Name,
    args: Seq[IR.DefinitionArgument],
    location: Location,
    typeSignature: Option[TypeSignatures.Metadata]
  ): Suggestion.Function = {
    typeSignature match {
      case Some(TypeSignatures.Signature(typeExpr)) =>
        val typeSig = buildTypeSignature(typeExpr)
        val (methodArgs, returnTypeDef) =
          buildFunctionArguments(args, typeSig)
        Suggestion.Function(
          externalId = externalId,
          module     = module,
          name       = name.name,
          arguments  = methodArgs,
          returnType = buildReturnType(returnTypeDef),
          scope      = buildScope(location)
        )
      case _ =>
        Suggestion.Function(
          externalId = externalId,
          module     = module,
          name       = name.name,
          arguments  = args.map(buildArgument),
          returnType = Any,
          scope      = buildScope(location)
        )
    }
  }

  /** Build a local suggestion. */
  private def buildLocal(
    externalId: Option[IR.ExternalId],
    module: String,
    name: String,
    location: Location,
    typeSignature: Option[TypeSignatures.Metadata]
  ): Suggestion.Local =
    typeSignature match {
      case Some(TypeSignatures.Signature(tname: IR.Name)) =>
        Suggestion.Local(
          externalId,
          module,
          name,
          tname.name,
          buildScope(location)
        )
      case _ =>
        Suggestion.Local(externalId, module, name, Any, buildScope(location))
    }

  /** Build suggestions for an atom definition. */
  private def buildAtom(
    module: String,
    name: String,
    arguments: Seq[IR.DefinitionArgument],
    doc: Option[String]
  ): Seq[Suggestion] =
    buildAtomConstructor(module, name, arguments, doc) +:
    buildAtomGetters(module, name, arguments)

  /** Build an atom constructor. */
  private def buildAtomConstructor(
    module: String,
    name: String,
    arguments: Seq[IR.DefinitionArgument],
    doc: Option[String]
  ): Suggestion.Atom =
    Suggestion.Atom(
      externalId    = None,
      module        = module,
      name          = name,
      arguments     = arguments.map(buildArgument),
      returnType    = name,
      documentation = doc
    )

  /** Build getter methods from atom arguments. */
  private def buildAtomGetters(
    module: String,
    name: String,
    arguments: Seq[IR.DefinitionArgument]
  ): Seq[Suggestion] =
    arguments.map { argument =>
      val thisArg = IR.DefinitionArgument.Specified(
        name         = IR.Name.This(argument.name.location),
        defaultValue = None,
        suspended    = false,
        location     = argument.location
      )
      buildMethod(
        externalId    = None,
        module        = module,
        name          = argument.name,
        selfType      = name,
        args          = Seq(thisArg),
        doc           = None,
        typeSignature = None
      )
    }

  /** Build self type from the method definitions metadata.
    *
    * @param definition the method definitions metadata
    * @return the self type
    */
  private def buildSelfType(
    definition: MethodDefinitions.Metadata
  ): Option[String] = {
    definition.target match {
      case BindingsMap.ResolvedModule(module) =>
        Some(module.getName.item)
      case BindingsMap.ResolvedConstructor(_, cons) =>
        Some(cons.name)
      case _ =>
        None
    }
  }

  /** Build type signature from the type expression.
    *
    * @param typeExpr the type signature expression
    * @return the list of type arguments
    */
  private def buildTypeSignature(typeExpr: IR.Expression): Vector[TypeArg] = {
    def go(typeExpr: IR.Expression, args: Vector[TypeArg]): Vector[TypeArg] =
      typeExpr match {
        case IR.Application.Prefix(_, args, _, _, _, _) =>
          args.toVector
            .map(arg => go(arg.value, Vector()))
            .map {
              case Vector(targ) => targ
              case targs        => TypeArg.Function(targs)
            }
        case IR.Function.Lambda(List(targ), body, _, _, _, _) =>
          val tdef = TypeArg.Value(targ.name.name, targ.suspended)
          go(body, args :+ tdef)
        case tname: IR.Name =>
          args :+ TypeArg.Value(tname.name, isSuspended = false)
        case _ =>
          args
      }

    go(typeExpr, Vector())
  }

  /** Build arguments of a method.
    *
    * @param vargs the list of value arguments
    * @param targs the list of type arguments
    * @param selfType the self type of a method
    * @return the list of arguments with a method return type
    */
  private def buildMethodArguments(
    vargs: Seq[IR.DefinitionArgument],
    targs: Seq[TypeArg],
    selfType: String
  ): (Seq[Suggestion.Argument], Option[TypeArg]) = {
    @scala.annotation.tailrec
    def go(
      vargs: Seq[IR.DefinitionArgument],
      targs: Seq[TypeArg],
      acc: Vector[Suggestion.Argument]
    ): (Vector[Suggestion.Argument], Option[TypeArg]) =
      if (vargs.isEmpty) {
        (acc, targs.lastOption)
      } else {
        vargs match {
          case IR.DefinitionArgument.Specified(
                name: IR.Name.This,
                defaultValue,
                suspended,
                _,
                _,
                _
              ) +: vtail =>
            val thisArg = Suggestion.Argument(
              name         = name.name,
              reprType     = selfType,
              isSuspended  = suspended,
              hasDefault   = defaultValue.isDefined,
              defaultValue = defaultValue.flatMap(buildDefaultValue)
            )
            go(vtail, targs, acc :+ thisArg)
          case varg +: vtail =>
            targs match {
              case targ +: ttail =>
                go(vtail, ttail, acc :+ buildTypedArgument(varg, targ))
              case _ =>
                go(vtail, targs, acc :+ buildArgument(varg))
            }
        }
      }

    go(vargs, targs, Vector())
  }

  /** Build arguments of a function.
    *
    * @param vargs the list of value arguments
    * @param targs the list of type arguments
    * @return the list of arguments with a function return type
    */
  private def buildFunctionArguments(
    vargs: Seq[IR.DefinitionArgument],
    targs: Seq[TypeArg]
  ): (Seq[Suggestion.Argument], Option[TypeArg]) = {
    @scala.annotation.tailrec
    def go(
      vargs: Seq[IR.DefinitionArgument],
      targs: Seq[TypeArg],
      acc: Vector[Suggestion.Argument]
    ): (Seq[Suggestion.Argument], Option[TypeArg]) =
      if (vargs.isEmpty) {
        (acc, targs.lastOption)
      } else {
        vargs match {
          case varg +: vtail =>
            targs match {
              case targ +: ttail =>
                go(vtail, ttail, acc :+ buildTypedArgument(varg, targ))
              case _ =>
                go(vtail, targs, acc :+ buildArgument(varg))
            }
        }
      }

    go(vargs, targs, Vector())
  }

  /** Build suggestion argument from a typed definition.
    *
    * @param varg the value argument
    * @param targ the type argument
    * @return the suggestion argument
    */
  private def buildTypedArgument(
    varg: IR.DefinitionArgument,
    targ: TypeArg
  ): Suggestion.Argument =
    Suggestion.Argument(
      name         = varg.name.name,
      reprType     = buildTypeArgumentName(targ),
      isSuspended  = buildTypeArgumentSuspendedFlag(targ),
      hasDefault   = varg.defaultValue.isDefined,
      defaultValue = varg.defaultValue.flatMap(buildDefaultValue)
    )

  /** Build the name of type argument.
    *
    * @param targ the type argument
    * @return the name of type argument
    */
  private def buildTypeArgumentName(targ: TypeArg): String = {
    def go(targ: TypeArg, level: Int): String =
      targ match {
        case TypeArg.Value(name, _) => name
        case TypeArg.Function(types) =>
          val typeList = types.map(go(_, level + 1))
          if (level > 0) typeList.mkString("(", " -> ", ")")
          else typeList.mkString(" -> ")
      }
    go(targ, 0)
  }

  /** Build the suspended flag of the type argument.
    *
    * @param targ the type argument
    * @return the suspended flag extracted from the type argument
    */
  private def buildTypeArgumentSuspendedFlag(targ: TypeArg): Boolean =
    targ match {
      case TypeArg.Value(_, isSuspended) => isSuspended
      case TypeArg.Function(_)           => false
    }

  /** Build suggestion argument from an untyped definition.
    *
    * @param arg the value argument
    * @return the suggestion argument
    */
  private def buildArgument(arg: IR.DefinitionArgument): Suggestion.Argument =
    Suggestion.Argument(
      name         = arg.name.name,
      reprType     = Any,
      isSuspended  = arg.suspended,
      hasDefault   = arg.defaultValue.isDefined,
      defaultValue = arg.defaultValue.flatMap(buildDefaultValue)
    )

  /** Build return type from the type definition.
    *
    * @param typeDef the type definition
    * @return the type name
    */
  private def buildReturnType(typeDef: Option[TypeArg]): String =
    typeDef.map(buildTypeArgumentName).getOrElse(Any)

  /** Build argument default value from the expression.
    *
    * @param expr the argument expression
    * @return the argument default value
    */
  private def buildDefaultValue(expr: IR): Option[String] =
    expr match {
      case IR.Literal.Number(value, _, _, _) => Some(value)
      case IR.Literal.Text(text, _, _, _)    => Some(text)
      case _                                 => None
    }

  /** Build scope from the location. */
  private def buildScope(location: Location): Suggestion.Scope =
    Suggestion.Scope(toPosition(location.start), toPosition(location.end))

  /** Convert absolute position index to the relative position of a suggestion.
    *
    * @param index the absolute position in the source
    * @return the relative position
    */
  private def toPosition(index: Int): Suggestion.Position = {
    val pos = IndexedSource[A].toPosition(index, source)
    Suggestion.Position(pos.line, pos.character)
  }
}

object SuggestionBuilder {

  /** Create the suggestion builder.
    *
    * @param source the text source
    * @tparam A the type of the text source
    */
  def apply[A: IndexedSource](source: A): SuggestionBuilder[A] =
    new SuggestionBuilder[A](source)

  /** A single level of an `IR`.
    *
    * @param queue the nodes in the scope
    * @param location the scope location
    */
  private case class Scope(queue: mutable.Queue[IR], location: Option[Location])

  private object Scope {

    /** Create new scope from the list of items.
      *
      * @param items the list of IR nodes
      * @param location the identified IR location
      * @return new scope
      */
    def apply(items: Seq[IR], location: Option[IR.IdentifiedLocation]): Scope =
      new Scope(mutable.Queue(items: _*), location.map(_.location))
  }

  /** The base trait for argument types. */
  sealed private trait TypeArg
  private object TypeArg {

    /** Type with the name, like `A`.
      *
      * @param name the name of the type
      * @param isSuspended is the argument lazy
      */
    case class Value(name: String, isSuspended: Boolean) extends TypeArg

    /** Function type, like `A -> A`.
      *
      * @param signature the list of types defining the function
      */
    case class Function(signature: Vector[TypeArg]) extends TypeArg

  }
  private val Any: String = "Any"

}
