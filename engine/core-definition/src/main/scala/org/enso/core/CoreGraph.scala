package org.enso.core

import org.enso.graph.definition.Macro.{component, field, genGraph, opaque}
import org.enso.graph.{Sized, VariantIndexed, Graph => PrimGraph}
import shapeless.{::, HNil}
import org.enso.syntax.text.AST

object CoreGraph {
  @genGraph object Definition {

    // ========================================================================
    // === The Graph Definition ===============================================
    // ========================================================================

    /** This type denotes the core graph itself. */
    case class CoreGraph() extends PrimGraph

    /** The list of components that make up a [[CoreGraph]]. */
    implicit def components =
      new PrimGraph.Component.List[CoreGraph] {
        type Out = Nodes :: Links :: HNil
      }

    // ========================================================================
    // === Opaque Storage =====================================================
    // ========================================================================

    /** Storage for string literals. */
    @opaque case class Literal(opaque: String)

    /** Storage for name literals. */
    @opaque case class Name(opaque: String)

    /** Storage for parents for a given node.
      *
      * An entry in the vector will be the index of an [[Edge]] in the graph
      * that has the containing node as its `target` field.
      */
    @opaque case class Parent(opaque: Vector[Int])

    /** Storage for raw AST nodes. */
    @opaque case class Ast(opaque: AST)

    // ========================================================================
    // === Node ===============================================================
    // ========================================================================

    /** A node in the [[CoreGraph]]. */
    @component case class Nodes() { type Node[G <: PrimGraph] }

    /** The list of fields that a [[Node]] has in a [[CoreGraph]]. */
    implicit def nodeFields =
      new PrimGraph.Component.Field.List[CoreGraph, Nodes] {
        type Out = Node.Shape :: Node.ParentLinks :: Node.Location :: HNil
      }

    object Node {

      // ======================================================================
      // === Field Definitions ================================================
      // ======================================================================

      /** A location describes which portion of the source code this particular
        * node in the graph represents.
        *
        * @param sourceStart the start position in the source code
        * @param sourceEnd the end position in the source code
        * @tparam G the graph type
        */
      @field case class Location[G <: PrimGraph](
        sourceStart: Int,
        sourceEnd: Int
      )

      /** This type represents all the incoming [[Link]]s to the current node.
        *
        * It should be noted that it _does not_ store the links directly. This
        * would only make sense if the link direction was reversed. Instead, it
        * holds unsafe references to the incoming link in the underlying graph.
        * These can be turned into the [[Link]]s directly by using
        * [[PrimGraph.GraphData.componentRefFromIndex()]].
        *
        * @param parents a vector containing the raw indices of the parent links
        * @tparam G the graph type
        */
      @field case class ParentLinks[G <: PrimGraph](
        parents: OpaqueData[Vector[Int], ParentStorage]
      )

      /** The shape of a node represents all the different forms that a node can
        * take.
        */
      @field object Shape {
        type G = PrimGraph

        // === Base Shapes ====================================================
        /** A representation of a node that has no particular shape. */
        case class Empty()

        /** A representation of a cons cell for building linked lists on the
          * graph.
          *
          * These should be used _very_ sparingly, if at all, but they provide a
          * way to store dynamically-sized core components providing they can be
          * broken down into statically sized components.
          *
          * The [[tail]] parameter should always point to either another node
          * with shape [[MetaList]] or a node with shape [[MetaNil]].
          *
          * It should be noted that, given that each [[Node]] contains a field
          * of [[ParentLinks]], that constructing this properly provides a
          * doubly-linked list, as no [[MetaList]] or [[MetaNil]] should have
          * more than one parent.
          *
          * @param head the current, arbitrary, element in the list
          * @param tail the rest of the list
          */
        case class MetaList(head: Link[G], tail: Link[G])

        /** A representation of the end of a linked-list on the graph. */
        case class MetaNil()

        /** A node representing boolean true. */
        case class MetaTrue()

        /** A node representing boolean false. */
        case class MetaFalse()

        // === Literals =======================================================

        /** A representation of a numeric literal.
          *
          * @param number the numeric literal
          */
        case class NumericLiteral(number: OpaqueData[String, LiteralStorage])

        /** A representation of a textual literal.
          *
          * @param text the textual literal
          */
        case class TextLiteral(text: OpaqueData[String, LiteralStorage])

        /** A representation of literal text from a foreign code block.
          *
          * @param code the foreign code literal
          */
        case class ForeignCodeLiteral(code: OpaqueData[String, LiteralStorage])

        // === Names ==========================================================

        /** A name.
          *
          * @param nameLiteral the literal string representing the name
          */
        case class Name(nameLiteral: OpaqueData[String, LiteralStorage])

        /** A representation of the `this` reserved name */
        case class ThisName()

        /** A representation of the `here` reserved name */
        case class HereName()

        // === Module =========================================================

        /** The core representation of a top-level Enso module.
          *
          * @param name        the name of the module
          * @param imports     the module's imports as a [[MetaList]], where
          *                    each list member points to an import
          * @param definitions the module's definitions as a [[MetaList]], where
          *                    each list member points to a binding
          */
        case class ModuleDef(
          name: Link[G],
          imports: Link[G],
          definitions: Link[G]
        )

        /** An import statement.
          *
          * @param segments the segments of the import path, represented as a
          *                 [[MetaList]].
          */
        case class Import(segments: Link[G])

        /** A module-level binding.
          *
          * @param module a link to the module in which this binding is found
          * @param binding the binding itself
          */
        case class TopLevelBinding(module: Link[G], binding: Link[G])

        // === Type Definitions ===============================================

        /** An atom definition.
          *
          * @param name the name of the atom
          * @param args the atom's arguments as a [[MetaList]]
          */
        case class AtomDef(name: Link[G], args: Link[G])

        /** An expanded-form type definition, with a body.
          *
          * @param name the name of the aggregate type
          * @param typeParams the type parameters to the definition, as a
          *                   [[MetaList]] of bindings
          * @param body       the body of the type definition, represented as a
          *                   [[MetaList]] of bindings
          */
        case class TypeDef(
          name: Link[G],
          typeParams: Link[G],
          body: Link[G]
        )

        // === Typing =========================================================

        /** The ascription of a type to a value.
          *
          * @param typed the expression being ascribed a type
          * @param sig the signature being ascribed to [[typed]]
          */
        case class TypeAscription(typed: Link[G], sig: Link[G])

        /** The `in` portion of a type signature that represents the monadic
          * contexts.
          *
          * @param typed the type being put in a context
          * @param context the context
          */
        case class ContextAscription(typed: Link[G], context: Link[G])

        /** A representation of a typeset member.
          *
          * PLEASE NOTE: This is here more as a note than anything, and will not
          * be exposed to users yet. It is currently used for Atom arguments.
          *
          * @param label the member's label, if given
          * @param memberType the member's type, if given
          * @param value the member's value, if given
          */
        case class TypesetMember(
          label: Link[G],
          memberType: Link[G],
          value: Link[G]
        )

        /** The typset subsumption judgement `<:`.
          *
          * This construct does not represent a user-facing language element.
          *
          * @param left the left type in the subsumption judgement
          * @param right the right type in the subsumption judgement
          */
        case class TypesetSubsumption(left: Link[G], right: Link[G])

        /** The typeset equality judgement `~`.
          *
          * This construct does not represent a user-facing language element.
          *
          * @param left the left operand
          * @param right the right operand
          */
        case class TypesetEquality(left: Link[G], right: Link[G])

        /** The typeset concatenation operator `,`.
          *
          * @param left the left operand
          * @param right the right operand
          */
        case class TypesetConcat(left: Link[G], right: Link[G])

        /** The typeset union operator `|`.
          *
          * @param left the left operand
          * @param right the right operand
          */
        case class TypesetUnion(left: Link[G], right: Link[G])

        /** The typeset intersection operator `&`.
          *
          * @param left the left operand
          * @param right the right operand
          */
        case class TypesetIntersection(left: Link[G], right: Link[G])

        /** The typeset subtraction operator `\`.
          *
          * @param left the left operand
          * @param right the right operand
          */
        case class TypesetSubtraction(left: Link[G], right: Link[G])

        // === Function =======================================================

        /** A lambda expression, the `->` function arrrow.
          *
          * Note that all lambdas in Enso are explicitly single-argument.
          *
          * @param arg the argument to the lambda
          * @param body the body of the lambda
          */
        case class Lambda(arg: Link[G], body: Link[G])

        /** A sugared function definition.
          *
          * @param name the name of the function
          * @param args the function arguments, as a [[MetaList]]
          * @param body the body of the function
          */
        case class FunctionDef(name: Link[G], args: Link[G], body: Link[G])

        /** A method definition.
          *
          * @param targetPath the path of the method
          * @param name the name of the method
          * @param function the function that is executed (can be any callable
          *                 representation)
          */
        case class MethodDef(
          targetPath: Link[G],
          name: Link[G],
          function: Link[G]
        )

        // === Definition-Site Argument Types =================================

        /** An ignored function argument, denoted by `_`.
          *
          * This can commonly be seen in use where an API requires a function
          * take an argument, but a particular implementation doesn't need it:
          * `_ -> ...`.
          */
        case class IgnoredArgument()

        /** A function argument definition.
          *
          * @param name      the name of the argument
          * @param suspended whether or not the argument uses suspended
          *                  evaluation (should be [[MetaTrue]] or [[MetaFalse]]
          * @param default   the default value for the argument, if present
          */
        case class DefinitionArgument(
          name: Link[G],
          suspended: Link[G],
          default: Link[G]
        )

        // === Applications ===================================================

        /** A function application.
          *
          * All functions in Enso are curried by default, and are represented in
          * the [[CoreGraph]] as single-argument functions.
          *
          * @param function function expression being called
          * @param argument the argument to the function
          */
        case class Application(function: Link[G], argument: Link[G])

        /** An infix function application.
          *
          * @param left the left argument
          * @param operator the function being applied
          * @param right the right argument
          */
        case class InfixApplication(
          left: Link[G],
          operator: Link[G],
          right: Link[G]
        )

        /** A left section operator application.
          *
          * @param arg the left argument to [[operator]]
          * @param operator the function being sectioned
          */
        case class LeftSection(arg: Link[G], operator: Link[G])

        /** A right section operator application.
          *
          * @param operator the function being sectioned
          * @param arg the right argument to [[operator]]
          */
        case class RightSection(operator: Link[G], arg: Link[G])

        /** A centre section operator application.
          *
          * @param operator the operator being sectioned
          */
        case class CentreSection(operator: Link[G])

        /** A representatin of a term that is explicitly forced.
          *
          * An explicitly forced term is one where the user has explicitly
          * called the `force` operator on it. This is useful only while the
          * compiler does not _automatically_ handle suspensions and forcing.
          *
          * PLEASE NOTE: This is temporary and will be removed as soon as the
          * compiler is capable enough to not require it.
          *
          * @param expression
          */
        case class ForcedTerm(expression: Link[G])

        // === Call-Site Argument Types =======================================

        /** Used to represent `_` arguments that are shorthand for the creation
          * of lambdas.
          */
        case class LambdaShorthandArgument()

        /** A function call-site argument.
          *
          * @param expression the argument expression
          * @param name the name of the argument, if given
          */
        case class CallSiteArgument(expression: Link[G], name: Link[G])

        /** The `...` argument that may be passed to a function to suspend the
          * execution of its default arguments.
          */
        case class SuspendDefaultsOperator()

        // === Structure ======================================================

        /** A block expression.
          *
          * @param expressions the expressions in the block as a [[MetaList]]
          * @param returnVal the final expression of the block
          */
        case class Block(expressions: Link[G], returnVal: Link[G])

        /** A binding expression of the form `name = expr`.
          *
          * @param name the name being bound to
          * @param expression the expression being bound to [[name]]
          */
        case class Binding(name: Link[G], expression: Link[G])

        // === Case Expression ================================================

        /** A case expression.
          *
          * @param scrutinee the case expression's scrutinee
          * @param branches the match branches, as a [[MetaList]]
          */
        case class CaseExpr(scrutinee: Link[G], branches: Link[G])

        /** A case branch.
          *
          * All case patterns will initially be desugared to a
          * [[StructuralPattern]] and will be refined during further desugaring
          * passes, some of which may depend on type checking.
          *
          * @param pattern the pattern to match the scrutinee against
          * @param expression the expression
          */
        case class CaseBranch(pattern: Link[G], expression: Link[G])

        /** A pattern that matches on the scrutinee based on its structure.
          *
          * @param matchExpression the expression representing the possible
          *                        structure of the scrutinee
          */
        case class StructuralPattern(matchExpression: Link[G])

        /** A pattern that matches on the scrutinee purely based on a type
          * subsumption judgement.
          *
          * @param matchExpression the expression representing the possible type
          *                        of the scrutinee
          */
        case class TypePattern(matchExpression: Link[G])

        /** A pattern that matches on the scrutinee based on a type subsumption
          * judgement and assigns a new name to it for use in the branch.
          *
          * @param matchExpression the expression representing the possible type
          *                        of the scrutinee, and its new name
          */
        case class NamedPattern(matchExpression: Link[G])

        /** A pattern that matches on any scrutinee. */
        case class FallbackPattern()

        // === Comments =======================================================

        /** A documentation comment.
          *
          * @param commented the commented entity
          * @param doc a [[TextLiteral]] containing the documentation comment
          */
        case class DocComment(commented: Link[G], doc: Link[G])

        // === Foreign ========================================================

        /** A foreign code definition.
          *
          * @param language the name of the foreign programming language
          * @param code the foreign code, represented as a [[ForeignCodeLiteral]]
          */
        case class ForeignDefinition(language: Link[G], code: Link[G])

        // === Errors =========================================================

        /** A syntax error.
          *
          * @param errorAst the raw AST representation of the syntax error
          */
        case class SyntaxError(errorAst: OpaqueData[AST, AstStorage])

        /** Returned on an attempt to construct erroneous core.
          *
          * @param erroneousCore a [[MetaList]] containing the one-or-more core
          *                      nodes that were in an incorrect format
          */
        case class ConstructionError(erroneousCore: Link[G])
      }

      // ======================================================================
      // === Utility Functions ================================================
      // ======================================================================

      /** Adds a link as a parent of the provided node.
        *
        * This should _only_ be used when the [[target]] field of [[link]]
        * points to [[node]].
        *
        * @param node the node to add a parent to
        * @param link the link to add as a parent
        * @param graph the graph in which this takes place
        * @param map the graph's parent storage
        */
      def addParent(
        node: Node[CoreGraph],
        link: Link[CoreGraph]
      )(
        implicit graph: PrimGraph.GraphData[CoreGraph],
        map: ParentStorage
      ): Unit = {
        import Node.ParentLinks._

        node.parents = node.parents :+ link.ix
      }

      /** Adds a node to the graph with its shape already set to a given shape.
        *
        * @param graph the graph to add the node to
        * @param ev evidence that the variant field is indexed
        * @tparam V the shape to set the node to
        * @return a refined node reference
        */
      def addRefined[V <: Node.Shape](
        implicit graph: PrimGraph.GraphData[CoreGraph],
        ev: VariantIndexed[Node.Shape, V]
      ): PrimGraph.Component.Refined[Node.Shape, V, Node[CoreGraph]] = {
        val node = graph.addNode()

        setShape[V](node)
        PrimGraph.Component.Refined[Node.Shape, V, Node[CoreGraph]](node)
      }

      /** Sets the shape of the provided [[node]] to [[NodeShape]].
        *
        * @param node the node to set
        * @param ev evidence that [[NodeShape]] belongs to an indexed variant
        * @param graph the graph to mutate
        * @tparam NodeShape the shape to set the node to
        */
      def setShape[NodeShape <: Node.Shape](
        node: Node[CoreGraph]
      )(
        implicit ev: VariantIndexed[Node.Shape, NodeShape],
        graph: PrimGraph.GraphData[CoreGraph]
      ): Unit = {
        graph.unsafeSetVariantCase[Nodes, Node.Shape, NodeShape](node)
      }

      /** Checks whether a given node represents some kind of language error.
        *
        * @param node the node to check
        * @return `true` if [[node]] represents an errors `false` otherwise
        */
      def isErrorNode(
        node: Node[CoreGraph]
      )(implicit graph: PrimGraph.GraphData[CoreGraph]): Boolean = {
        node match {
          case Shape.SyntaxError.any(_)       => true
          case Shape.ConstructionError.any(_) => true
          case _                              => false
        }
      }

      /** Checks whether a given node represents syntactic sugar.
        *
        * @param node the node to check
        * @return `true` if [[node]] represents syntax sugar, `false` otherwise
        */
      def shapeIsSugar(
        node: Node[CoreGraph]
      )(implicit graph: PrimGraph.GraphData[CoreGraph]): Boolean = {
        node match {
          case Shape.TypeDef.any(_)          => true
          case Shape.FunctionDef.any(_)      => true
          case Shape.InfixApplication.any(_) => true
          case Shape.LeftSection.any(_)      => true
          case Shape.RightSection.any(_)     => true
          case Shape.CentreSection.any(_)    => true
          case Shape.ForcedTerm.any(_)       => true
          case _                             => false
        }
      }

      /** Checks whether a given node represents primitive language constructs.
        *
        * @param node the node to check
        * @return `true` if [[Node]] has a primitive shape, `false` otherwise
        */
      def shapeIsPrimitive(
        node: Node[CoreGraph]
      )(implicit graph: PrimGraph.GraphData[CoreGraph]): Boolean = {
        !shapeIsSugar(node)
      }
    }

    // ========================================================================
    // === Link ===============================================================
    // ========================================================================

    /** A link between nodes in the [[CoreGraph]]. */
    @component case class Links() { type Link[G <: PrimGraph] }

    /** The list of fields that a [[Link]] has in a [[CoreGraph]]. */
    implicit def linkFields =
      new PrimGraph.Component.Field.List[CoreGraph, Links] {
        type Out = Link.Shape :: HNil
      }

    object Link {

      // ======================================================================
      // === Field Definitions ================================================
      // ======================================================================

      /** The shape of a link is static and represents a standard directional
        * edge in a graph.
        *
        * @param source the node at the start of the link
        * @param target the node at the end of the link
        * @tparam G the graph type
        */
      @field case class Shape[G <: PrimGraph](source: Node[G], target: Node[G])
    }
  }
}
