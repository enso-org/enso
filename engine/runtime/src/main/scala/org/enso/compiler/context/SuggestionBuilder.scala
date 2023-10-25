package org.enso.compiler.context

import org.enso.compiler.Compiler
import org.enso.compiler.context.CompilerContext
import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.{ExternalID, IR}
import org.enso.compiler.core.ir.expression.{Application, Operator}
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  Expression,
  Function,
  IdentifiedLocation,
  Literal,
  Name,
  Type
}
import org.enso.compiler.core.ir.module.scope.Definition
import org.enso.compiler.core.ir.module.scope.definition
import org.enso.compiler.core.ir.`type`
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.resolve.{
  DocumentationComments,
  GenericAnnotations,
  MethodDefinitions,
  TypeNames,
  TypeSignatures
}
import org.enso.interpreter.runtime.`type`.Types
import org.enso.pkg.QualifiedName
import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.{Tree, TypeGraph}
import org.enso.syntax.text.Location
import org.enso.text.editing.IndexedSource

import java.util.UUID
import scala.collection.mutable

/** Module that extracts [[Suggestion]] entries from the [[IR]].
  *
  * @param source the text source
  * @param typeGraph the type hierarchy
  * @param compiler the compiler instance
  * @tparam A the type of the text source
  */
final class SuggestionBuilder[A: IndexedSource](
  val source: A,
  val typeGraph: TypeGraph,
  val compiler: Compiler
) {

  import SuggestionBuilder._

  /** Build suggestions from the given `ir`.
    *
    * @param module the module name
    * @param ir the input `IR`
    * @return the tree of suggestion entries extracted from the given `IR`
    */
  def build(module: QualifiedName, ir: IR): Tree.Root[Suggestion] = {
    type TreeBuilder =
      mutable.Builder[Tree.Node[Suggestion], Vector[Tree.Node[Suggestion]]]

    def go(tree: TreeBuilder, scope: Scope): Vector[Tree.Node[Suggestion]] = {
      if (scope.queue.isEmpty) {
        tree.result()
      } else {
        val ir  = scope.queue.dequeue()
        val doc = ir.getMetadata(DocumentationComments).map(_.documentation)
        ir match {
          case Definition.Type(
                tpName,
                params,
                List(),
                _,
                _,
                _
              ) =>
            val tpe =
              buildAtomType(module, tpName.name, tpName.name, params, doc)
            go(tree ++= Vector(Tree.Node(tpe, Vector())), scope)

          case Definition.Type(
                tpName,
                params,
                members,
                _,
                _,
                _
              ) =>
            val tpe =
              buildAtomType(module, tpName.name, tpName.name, params, doc)
            val conses = members.map {
              case data @ Definition.Data(
                    name,
                    arguments,
                    annotations,
                    _,
                    _,
                    _
                  ) =>
                buildAtomConstructor(
                  module,
                  tpName.name,
                  name.name,
                  arguments,
                  annotations,
                  data.getMetadata(DocumentationComments).map(_.documentation)
                )
            }
            val getters = members
              .flatMap(_.arguments)
              .filterNot { argument =>
                argument.name.name.startsWith(InternalPrefix) ||
                argument.name.name.endsWith(InternalSuffix)
              }
              .distinctBy(_.name.name)
              .map(buildGetter(module, tpName.name, _))

            val tpSuggestions = tpe +: conses ++: getters

            go(tree ++= tpSuggestions.map(Tree.Node(_, Vector())), scope)

          case m @ definition.Method
                .Explicit(
                  Name.MethodReference(typePtr, methodName, _, _, _),
                  Function.Lambda(args, body, _, _, _, _),
                  _,
                  _,
                  _
                ) if !m.isStaticWrapperForInstanceMethod =>
            val typeSignature = ir.getMetadata(TypeSignatures)
            val annotations   = ir.getMetadata(GenericAnnotations)
            val (selfTypeOpt, isStatic) = typePtr match {
              case Some(typePtr) =>
                val selfType = typePtr
                  .getMetadata(MethodDefinitions)
                  .map(_.target.qualifiedName)
                selfType -> m.isStatic
              case None =>
                Some(module) -> true
            }
            val methodOpt = selfTypeOpt.map { selfType =>
              buildMethod(
                body.getExternalId,
                module,
                methodName.name,
                selfType,
                isStatic,
                args,
                doc,
                typeSignature,
                annotations,
                MethodType.Defined
              )
            }
            val subforest = go(
              Vector.newBuilder,
              Scope(body.children, body.location)
            )
            go(tree ++= methodOpt.map(Tree.Node(_, subforest)), scope)

          case definition.Method
                .Conversion(
                  Name.MethodReference(typePtr, _, _, _, _),
                  _,
                  Function.Lambda(args, body, _, _, _, _),
                  _,
                  _,
                  _
                ) =>
            val selfType = typePtr.flatMap { typePointer =>
              typePointer
                .getMetadata(MethodDefinitions)
                .map(_.target.qualifiedName)
            }
            val conversion = buildConversion(
              body.getExternalId,
              module,
              selfType,
              args,
              doc
            )
            go(tree += Tree.Node(conversion, Vector()), scope)

          case Expression.Binding(
                name,
                Function.Lambda(args, body, _, _, _, _),
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
              doc,
              typeSignature
            )
            val subforest = go(
              Vector.newBuilder,
              Scope(body.children, body.location)
            )
            go(tree += Tree.Node(function, subforest), scope)

          case Expression.Binding(name, expr, _, _, _)
              if name.location.isDefined =>
            val typeSignature = ir.getMetadata(TypeSignatures)
            val local = buildLocal(
              expr.getExternalId,
              module,
              name.name,
              scope.location.get,
              doc,
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

    val builder: TreeBuilder = Vector.newBuilder
    builder += Tree.Node(
      buildModule(
        module,
        ir.getMetadata(DocumentationComments).map(_.documentation)
      ),
      Vector()
    )

    Tree.Root(
      go(builder, Scope(ir.children, ir.location))
    )
  }

  /** Build a method suggestion. */
  private def buildMethod(
    externalId: Option[UUID @ExternalID],
    module: QualifiedName,
    name: String,
    selfType: QualifiedName,
    isStatic: Boolean,
    args: Seq[DefinitionArgument],
    doc: Option[String],
    typeSignature: Option[TypeSignatures.Metadata],
    genericAnnotations: Option[GenericAnnotations.Metadata],
    methodType: MethodType
  ): Suggestion.Method = {
    val typeSig = buildTypeSignatureFromMetadata(typeSignature)
    val (methodArgs, returnTypeDef) =
      buildMethodArguments(args, typeSig, selfType, isStatic)
    val annotations =
      genericAnnotations.map(buildAnnotationsFromMetadata).getOrElse(Seq())
    methodType match {
      case MethodType.Getter =>
        Suggestion.Getter(
          externalId    = externalId,
          module        = module.toString,
          name          = name,
          arguments     = methodArgs,
          selfType      = selfType.toString,
          returnType    = buildReturnType(returnTypeDef),
          documentation = doc,
          annotations   = annotations
        )
      case MethodType.Defined =>
        Suggestion.DefinedMethod(
          externalId    = externalId,
          module        = module.toString,
          name          = name,
          arguments     = methodArgs,
          selfType      = selfType.toString,
          returnType    = buildReturnType(returnTypeDef),
          isStatic      = isStatic,
          documentation = doc,
          annotations   = annotations
        )
    }
  }

  /** Build a conversion suggestion. */
  private def buildConversion(
    externalId: Option[UUID @ExternalID],
    module: QualifiedName,
    selfType: Option[QualifiedName],
    args: Seq[DefinitionArgument],
    doc: Option[String]
  ): Suggestion.Conversion = {
    val methodArgs =
      args.map { arg =>
        buildTypeSignatureFromMetadata(
          arg.getMetadata(TypeSignatures)
        ).headOption
          .map(buildTypedArgument(arg, _))
          .getOrElse(buildArgument(arg))
      }.tail

    Suggestion.Conversion(
      externalId    = externalId,
      module        = module.toString,
      arguments     = methodArgs,
      selfType      = methodArgs.head.reprType,
      returnType    = selfType.fold(Any)(_.toString),
      documentation = doc
    )
  }

  /** Build a function suggestion. */
  private def buildFunction(
    externalId: Option[UUID @ExternalID],
    module: QualifiedName,
    name: Name,
    args: Seq[DefinitionArgument],
    location: Location,
    doc: Option[String],
    typeSignature: Option[TypeSignatures.Metadata]
  ): Suggestion.Function = {
    val typeSig = buildTypeSignatureFromMetadata(typeSignature)
    val (methodArgs, returnTypeDef) =
      buildFunctionArguments(args, typeSig)
    Suggestion.Function(
      externalId    = externalId,
      module        = module.toString,
      name          = name.name,
      arguments     = methodArgs,
      returnType    = buildReturnType(returnTypeDef),
      scope         = buildScope(location),
      documentation = doc
    )
  }

  /** Build a local suggestion. */
  private def buildLocal(
    externalId: Option[UUID @ExternalID],
    module: QualifiedName,
    name: String,
    location: Location,
    doc: Option[String],
    typeSignature: Option[TypeSignatures.Metadata]
  ): Suggestion.Local = {
    val typeSig            = buildTypeSignatureFromMetadata(typeSignature)
    val (_, returnTypeDef) = buildFunctionArguments(Seq(), typeSig)
    Suggestion.Local(
      externalId    = externalId,
      module        = module.toString,
      name          = name,
      returnType    = buildReturnType(returnTypeDef),
      scope         = buildScope(location),
      documentation = doc
    )
  }

  /** Build an atom suggestion representing a module. */
  private def buildModule(
    module: QualifiedName,
    doc: Option[String]
  ): Suggestion =
    Suggestion.Module(
      module        = module.toString,
      documentation = doc
    )

  /** Build a type suggestion. */
  private def buildAtomType(
    module: QualifiedName,
    tp: String,
    name: String,
    params: Seq[DefinitionArgument],
    doc: Option[String]
  ): Suggestion.Type = {
    val qualifiedName = module.createChild(tp).toString
    val parentType    = typeGraph.getDirectParents(qualifiedName).headOption
    Suggestion.Type(
      externalId    = None,
      module        = module.toString,
      name          = name,
      params        = params.map(buildArgument),
      returnType    = qualifiedName,
      parentType    = parentType,
      documentation = doc
    )
  }

  /** Build an atom constructor. */
  private def buildAtomConstructor(
    module: QualifiedName,
    tp: String,
    name: String,
    arguments: Seq[DefinitionArgument],
    genericAnnotations: Seq[Name.GenericAnnotation],
    doc: Option[String]
  ): Suggestion.Constructor =
    Suggestion.Constructor(
      externalId    = None,
      module        = module.toString,
      name          = name,
      arguments     = arguments.map(buildArgument),
      returnType    = module.createChild(tp).toString,
      documentation = doc,
      annotations   = genericAnnotations.map(_.name)
    )

  /** Build getter methods from atom arguments. */
  private def buildGetter(
    module: QualifiedName,
    typeName: String,
    argument: DefinitionArgument
  ): Suggestion = {
    val getterName = argument.name.name
    val thisArg = DefinitionArgument.Specified(
      name         = Name.Self(None),
      ascribedType = None,
      defaultValue = None,
      suspended    = false,
      location     = None
    )
    buildMethod(
      externalId         = None,
      module             = module,
      name               = getterName,
      selfType           = module.createChild(typeName),
      isStatic           = false,
      args               = Seq(thisArg),
      doc                = None,
      typeSignature      = argument.name.getMetadata(TypeSignatures),
      genericAnnotations = None,
      methodType         = MethodType.Getter
    )
  }

  /** Build a [[TypeArg]] from the resolved type name.
    *
    * @param resolvedName the resolved type name
    * @return the corresponding type argument
    */
  private def buildResolvedTypeName(
    resolvedName: BindingsMap.ResolvedName
  ): TypeArg =
    resolvedName match {
      case tp: BindingsMap.ResolvedType =>
        tp.getVariants.size match {
          case 0 =>
            val isBuiltinTypeWithValues =
              tp.tp.builtinType &&
              compiler.context.typeContainsValues(tp.tp.name)
            if (isBuiltinTypeWithValues) {
              TypeArg.Sum(Some(tp.qualifiedName), Seq())
            } else {
              TypeArg.Sum(
                Some(tp.qualifiedName),
                Seq(TypeArg.Value(tp.qualifiedName))
              )
            }

          case 1 =>
            TypeArg.Sum(Some(tp.qualifiedName), Seq())

          case _ =>
            TypeArg.Sum(
              Some(tp.qualifiedName),
              tp.getVariants.map(r => TypeArg.Value(r.qualifiedName))
            )
        }
      case _: BindingsMap.ResolvedName =>
        TypeArg.Value(resolvedName.qualifiedName)
    }

  /** Build annotations from metadata. */
  private def buildAnnotationsFromMetadata(
    genericAnnotations: GenericAnnotations.Metadata
  ): Seq[String] =
    genericAnnotations.annotations.map(_.name)

  /** Build type signature from the ir metadata.
    *
    * @param typeSignature the type signature metadata
    * @return the list of type arguments
    */
  private def buildTypeSignatureFromMetadata(
    typeSignature: Option[TypeSignatures.Metadata]
  ): Vector[TypeArg] =
    typeSignature match {
      case Some(TypeSignatures.Signature(typeExpr)) =>
        buildTypeSignature(typeExpr)
      case _ =>
        Vector()
    }

  /** Build type signature from the type expression.
    *
    * @param typeExpr the type signature expression
    * @return the list of type arguments
    */
  private def buildTypeSignature(
    typeExpr: Expression
  ): Vector[TypeArg] = {
    def go(expr: Expression): TypeArg = expr match {
      case fn: Type.Function =>
        TypeArg.Function(fn.args.map(go).toVector, go(fn.result))
      case union: `type`.Set.Union =>
        TypeArg.Sum(None, union.operands.map(go))
      case app: Application.Prefix =>
        TypeArg.Application(
          go(app.function),
          app.arguments.map(c => go(c.value)).toVector
        )
      case bin: Operator.Binary =>
        TypeArg.Binary(
          go(bin.left.value),
          go(bin.right.value),
          bin.operator.name
        )
      case tname: Name =>
        tname
          .getMetadata(TypeNames)
          .map(t => buildResolvedTypeName(t.target))
          .getOrElse(TypeArg.Value(QualifiedName.simpleName(tname.name)))

      case _ =>
        TypeArg.Value(QualifiedName.fromString(Any))
    }
    val r = go(typeExpr)
    r match {
      case fn: TypeArg.Function => fn.arguments :+ fn.result
      case _                    => Vector(r)
    }
  }

  /** Build arguments of a method.
    *
    * @param vargs the list of value arguments
    * @param targs the list of type arguments
    * @param selfType the self type of a method
    * @param isStatic is the method static
    * @return the list of arguments with a method return type
    */
  private def buildMethodArguments(
    vargs: Seq[DefinitionArgument],
    targs: Seq[TypeArg],
    selfType: QualifiedName,
    isStatic: Boolean
  ): (Seq[Suggestion.Argument], Option[TypeArg]) = {
    @scala.annotation.tailrec
    def go(
      vargs: Seq[DefinitionArgument],
      targs: Seq[TypeArg],
      acc: Vector[Suggestion.Argument]
    ): (Vector[Suggestion.Argument], Option[TypeArg]) =
      if (vargs.isEmpty) {
        (acc, targs.lastOption)
      } else {
        vargs match {
          case DefinitionArgument.Specified(
                name: Name.Self,
                _,
                defaultValue,
                suspended,
                _,
                _,
                _
              ) +: vtail =>
            if (isStatic) {
              go(vtail, targs, acc)
            } else {
              val thisArg = Suggestion.Argument(
                name         = name.name,
                reprType     = selfType.toString,
                isSuspended  = suspended,
                hasDefault   = defaultValue.isDefined,
                defaultValue = defaultValue.flatMap(buildDefaultValue)
              )
              go(vtail, targs, acc :+ thisArg)
            }
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
    vargs: Seq[DefinitionArgument],
    targs: Seq[TypeArg]
  ): (Seq[Suggestion.Argument], Option[TypeArg]) = {
    @scala.annotation.tailrec
    def go(
      vargs: Seq[DefinitionArgument],
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
    varg: DefinitionArgument,
    targ: TypeArg
  ): Suggestion.Argument =
    Suggestion.Argument(
      name         = varg.name.name,
      reprType     = buildTypeArgumentName(targ),
      isSuspended  = varg.suspended,
      hasDefault   = varg.defaultValue.isDefined,
      defaultValue = varg.defaultValue.flatMap(buildDefaultValue),
      tagValues    = buildTagValues(targ)
    )

  /** Build tag values of type argument.
    *
    * @param targ the type argument
    * @return the list of tag values
    */
  private def buildTagValues(targ: TypeArg): Option[Seq[String]] = {
    def go(arg: TypeArg): Seq[String] = arg match {
      case TypeArg.Sum(_, List())   => Seq()
      case TypeArg.Sum(_, variants) => variants.flatMap(go)
      case TypeArg.Value(n)         => Seq(n.toString)
      case _                        => Seq()
    }

    targ match {
      case s: TypeArg.Sum =>
        val tagValues = go(s)
        Option.unless(tagValues.isEmpty)(tagValues)
      case _ => None

    }
  }

  /** Build the name of type argument.
    *
    * @param targ the type argument
    * @return the name of type argument
    */
  private def buildTypeArgumentName(targ: TypeArg): String = {
    def go(targ: TypeArg, level: Int): String =
      targ match {
        case TypeArg.Value(name) => name.toString
        case TypeArg.Function(args, ret) =>
          val types    = args :+ ret
          val typeList = types.map(go(_, level + 1))
          if (level > 0) typeList.mkString("(", " -> ", ")")
          else typeList.mkString(" -> ")
        case TypeArg.Binary(l, r, op) =>
          val left  = go(l, level + 1)
          val right = go(r, level + 1)
          s"$left $op $right"
        case TypeArg.Application(fun, args) =>
          val funText  = go(fun, level)
          val argsList = args.map(go(_, level + 1)).mkString(" ")
          val typeName = s"$funText $argsList"
          if (level > 0) s"($typeName)" else typeName
        case TypeArg.Sum(Some(n), _) => n.toString
        case TypeArg.Sum(None, variants) =>
          variants.map(go(_, level + 1)).mkString(" | ")
      }

    go(targ, 0)
  }

  /** Build suggestion argument from an untyped definition.
    *
    * @param arg the value argument
    * @return the suggestion argument
    */
  private def buildArgument(arg: DefinitionArgument): Suggestion.Argument = {
    buildTypeSignatureFromMetadata(arg.name.getMetadata(TypeSignatures)) match {
      case Vector(targ) =>
        buildTypedArgument(arg, targ)
      case _ =>
        Suggestion.Argument(
          name         = arg.name.name,
          reprType     = Any,
          isSuspended  = arg.suspended,
          hasDefault   = arg.defaultValue.isDefined,
          defaultValue = arg.defaultValue.flatMap(buildDefaultValue)
        )
    }
  }

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
      case Literal.Number(_, value, _, _, _) => Some(value)
      case Literal.Text(text, _, _, _)       => Some(text)
      case Application.Prefix(name, path, _, _, _, _) =>
        Some(path.map(_.value.showCode()).mkString(".") + "." + name.showCode())
      case other => Some(other.showCode())
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

  /** Creates the suggestion builder for a module.
    *
    * @param module the module to index
    * @param compiler the compiler instance
    * @return the suggestions builder for the module
    */
  def apply(
    module: CompilerContext.Module,
    compiler: Compiler
  ): SuggestionBuilder[CharSequence] =
    SuggestionBuilder(module.getSource.getCharacters, compiler)

  /** Create the suggestion builder.
    *
    * @param source the text source
    * @param typeGraph the type hierarchy
    * @param compiler the compiler instance
    * @tparam A the type of the text source
    */
  def apply[A: IndexedSource](
    source: A,
    typeGraph: TypeGraph,
    compiler: Compiler
  ): SuggestionBuilder[A] =
    new SuggestionBuilder[A](source, typeGraph, compiler)

  /** Create the suggestion builder.
    *
    * @param source the text source
    * @param compiler the compiler instance
    * @tparam A the type of the text source
    */
  def apply[A: IndexedSource](
    source: A,
    compiler: Compiler
  ): SuggestionBuilder[A] =
    new SuggestionBuilder[A](source, Types.getTypeHierarchy, compiler)

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
    def apply(items: Seq[IR], location: Option[IdentifiedLocation]): Scope =
      new Scope(mutable.Queue(items: _*), location.map(_.location))
  }

  /** The base trait for argument types. */
  sealed private trait TypeArg
  private object TypeArg {

    /** A sum type – one of many possible options.
      * @param name the qualified name of the type.
      * @param variants the qualified names of constituent atoms.
      */
    case class Sum(name: Option[QualifiedName], variants: Seq[TypeArg])
        extends TypeArg

    /** Type with the name, like `A`.
      *
      * @param name the name of the type
      */
    case class Value(name: QualifiedName) extends TypeArg

    /** Function type, like `A -> A`.
      *
      * @param arguments the list of types defining the function
      */
    case class Function(arguments: Vector[TypeArg], result: TypeArg)
        extends TypeArg

    /** Binary operator, like `A | B`
      *
      * @param left the left hand side of a binary operator
      * @param right the right hand side of a binary operator
      * @param operator the binary operator
      */
    case class Binary(left: TypeArg, right: TypeArg, operator: String)
        extends TypeArg

    /** Function application, like `Either A B`.
      *
      * @param function the function type
      * @param arguments the list of argument types
      */
    case class Application(
      function: TypeArg,
      arguments: Vector[TypeArg]
    ) extends TypeArg
  }

  /** Base trait for method types. */
  sealed private trait MethodType
  private object MethodType {
    case object Getter  extends MethodType
    case object Defined extends MethodType
  }

  val Any: String = "Standard.Base.Any.Any"

  private val InternalSuffix = "_internal"
  private val InternalPrefix = "internal_"

}
