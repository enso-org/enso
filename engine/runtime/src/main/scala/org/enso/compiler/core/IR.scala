package org.enso.compiler.core

import java.util.UUID

import org.enso.compiler.core.IR.{
  DiagnosticStorage,
  Expression,
  IdentifiedLocation
}
import org.enso.compiler.core.ir.MetadataStorage
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.{AST, Debug, Location}

/** [[IR]] is a temporary and fairly unsophisticated internal representation
  * format for Enso programs.
  *
  * It is a purely tree-based representation to support basic desugaring and
  * analysis passes that do not rely on the ability to create cycles in the IR
  * itself. Its existence is the natural evolution of the older AstExpression
  * format used during the initial development of the interpreter.
  *
  * In time, it will be replaced by [[Core]], but expediency dictates that we
  * retain and evolve this representation for the near future.
  *
  * Please note that all extensions of [[IR]] must reimplement `copy` to keep
  * the id intact when copying nodes. The copy implementation should provide a
  * way to set the id for the copy, but should default to being copied. Care
  * must be taken to not end up with two nodes with the same ID. When using
  * `copy` to duplicate nodes, please ensure that a new ID is provided.
  */
sealed trait IR {

  /** Storage for metadata that the node has been tagged with as the result of
    * various compiler passes.
    */
  val passData: MetadataStorage

  /** The source location that the node corresponds to. */
  val location: Option[IdentifiedLocation]

  /** Gets the external identifier from an IR node, if it is present.
    *
    * @return the external identifier for this IR node
    */
  def getExternalId: Option[IR.ExternalId] = {
    location.flatMap(l => l.id)
  }

  /** Maps the provided function over any expression defined as a child of the
    * node this is called on.
    *
    * @param fn the function to transform the expressions
    * @return `this`, potentially having had its children transformed by `fn`
    */
  def mapExpressions(fn: Expression => Expression): IR

  /** Gets the list of all children IR nodes of this node.
    *
    * @return this node's children.
    */
  def children: List[IR]

  /** Lists all the nodes in the preorder walk of the tree of this node.
    *
    * @return all the descendants of this node.
    */
  def preorder: List[IR] = this :: children.flatMap(_.preorder)

  /** Pretty prints the IR.
    *
    * @return a pretty-printed representation of the IR
    */
  def pretty: String = Debug.pretty(this.toString)

  /** Gets the node's identifier.
    *
    * @return the node's identifier
    */
  def getId: IR.Identifier = id

  /** A unique identifier for a piece of IR. */
  protected var id: IR.Identifier

  /** Storage for compiler diagnostics related to the IR node. */
  val diagnostics: DiagnosticStorage
}
object IR {

  /** Creates a random identifier.
    *
    * @return a random identifier
    */
  def randomId: IR.Identifier = {
    UUID.randomUUID()
  }

  /** The type of identifiers for IR nodes. */
  type Identifier = UUID

  /** The type of external identifiers */
  type ExternalId = AST.ID

  /**
    * Couples a location with a possible source identifier.
    *
    * @param location the code location.
    * @param id the identifier for the location.
    */
  case class IdentifiedLocation(location: Location, id: Option[AST.ID]) {

    /**
      * @return the character index of the start of this source location.
      */
    def start: Int = location.start

    /**
      * @return the character index of the end of this source location.
      */
    def end: Int = location.end

    /**
      * @return the length in characters of this location.
      */
    def length: Int = location.length
  }
  object IdentifiedLocation {

    /**
      * Utility constructor, building a location without an ID.
      *
      * @param location the code location.
      * @return an [[IdentifiedLocation]] corresponding to the input location.
      */
    def apply(location: Location): IdentifiedLocation =
      IdentifiedLocation(location, None)
  }

  // === Basic Shapes =========================================================

  /** A node representing an empty IR construct that can be used in any place.
    *
    * @param location the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Empty(
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends IR
      with Expression
      with Diagnostic
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id the identifier for the new node
      * @return a copy of `this` with the specified fields updated
      */
    def copy(
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: Identifier                       = id
    ): Empty = {
      val res = Empty(location, passData, diagnostics)
      res.id = id
      res
    }

    override def mapExpressions(fn: Expression => Expression): Empty = this

    override def toString: String =
      s"""
      |IR.Empty(
      |location = $location,
      |passData = ${this.showPassData},
      |diagnostics = $diagnostics,
      |id = $id
      |)
      |""".toSingleLine

    override def children: List[IR] = List()

    override def message: String =
      "Empty IR: Please report this as a compiler bug."
  }

  // === Module ===============================================================

  /** A representation of a top-level Enso module.
    *
    * Modules may only contain imports and top-level bindings, with no top-level
    * executable code.
    *
    * @param imports the import statements that bring other modules into scope
    * @param bindings the top-level bindings for this module
    * @param location the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Module(
    imports: List[Module.Scope.Import],
    bindings: List[Module.Scope.Definition],
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends IR
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param imports the import statements that bring other modules into scope
      * @param bindings the top-level bindings for this module
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      imports: List[Module.Scope.Import]      = imports,
      bindings: List[Module.Scope.Definition] = bindings,
      location: Option[IdentifiedLocation]    = location,
      passData: MetadataStorage               = passData,
      diagnostics: DiagnosticStorage          = diagnostics,
      id: Identifier                          = id
    ): Module = {
      val res = Module(imports, bindings, location, passData, diagnostics)
      res.id = id
      res
    }

    override def mapExpressions(fn: Expression => Expression): Module = {
      copy(
        imports  = imports.map(_.mapExpressions(fn)),
        bindings = bindings.map(_.mapExpressions(fn))
      )
    }

    override def children: List[IR] = imports ++ bindings

    def transformExpressions(
      fn: PartialFunction[Expression, Expression]
    ): Module = {
      copy(
        bindings = bindings.map(_.mapExpressions(_.transformExpressions(fn)))
      )
    }

    override def toString: String =
      s"""
      |IR.Module(
      |imports = $imports,
      |bindings = $bindings,
      |location = $location,
      |passData = ${this.showPassData},
      |diagnostics = $diagnostics,
      |id = $id
      |)
      |""".toSingleLine
  }
  object Module {

    /** A representation of constructs that can only occur in the top-level
      * module scope
      */
    sealed trait Scope extends IR {
      override def mapExpressions(fn: Expression => Expression): Scope
    }
    object Scope {

      /** Module-level import statements. */
      sealed trait Import extends Scope {
        override def mapExpressions(fn: Expression => Expression): Import
      }

      object Import {

        /** An import statement.
          *
          * @param name the full `.`-separated path representing the import
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Module(
          name: String,
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Import
            with IRKind.Primitive {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param name the full `.`-separated path representing the import
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            name: String                         = name,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Module = {
            val res = Module(name, location, passData, diagnostics)
            res.id = id
            res
          }

          override def mapExpressions(
            fn: Expression => Expression
          ): Module = this

          override def toString: String =
            s"""
            |IR.Module.Scope.Import.Module(
            |name = $name,
            |location = $location,
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List()
        }

        object Polyglot {

          /** Represents language-specific polyglot import data. */
          sealed trait Entity

          /** Represents an import of a Java class.
            *
            * @param packageName the name of the package containing the imported
            *                    class
            * @param className the class name
            */
          case class Java(packageName: String, className: String) extends Entity
        }

        /** An import of a polyglot class.
          *
          * @param entity language-specific information on the imported entity
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Polyglot(
          entity: Polyglot.Entity,
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Import
            with IRKind.Primitive {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param entity language-specific information on the imported entity
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            entity: Polyglot.Entity,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Polyglot = {
            val res =
              Polyglot(entity, location, passData, diagnostics)
            res.id = id
            res
          }

          override def mapExpressions(fn: Expression => Expression): Polyglot =
            this

          override def toString: String =
            s"""
            |IR.Module.Scope.Import.Polyglot(
            |entity = $entity,
            |location = $location,
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List()
        }
      }

      /** A representation of top-level definitions. */
      sealed trait Definition extends Scope {
        override def mapExpressions(fn: Expression => Expression): Definition
      }
      object Definition {

        /** The definition of an atom constructor and its associated arguments.
          *
          * @param name the name of the atom
          * @param arguments the arguments to the atom constructor
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Atom(
          name: IR.Name,
          arguments: List[DefinitionArgument],
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Definition
            with IRKind.Primitive {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param name the name of the atom
            * @param arguments the arguments to the atom constructor
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            name: IR.Name                        = name,
            arguments: List[DefinitionArgument]  = arguments,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Atom = {
            val res = Atom(name, arguments, location, passData, diagnostics)
            res.id = id
            res
          }

          override def mapExpressions(fn: Expression => Expression): Atom = {
            copy(
              name      = name.mapExpressions(fn),
              arguments = arguments.map(_.mapExpressions(fn))
            )
          }

          override def toString: String =
            s"""
            |IR.Module.Scope.Definition.Atom(
            |name = $name,
            |arguments = $arguments,
            |location = $location,
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = name :: arguments
        }

        /** The definition of a method for a given constructor [[typeName]].
          *
          * @param typeName the name of the atom that the method is being
          *                 defined for
          * @param methodName the name of the method being defined on `typename`
          * @param body the body of the method
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        // TODO [AA] Separate Method into Method.Binding and Method.Explicit to
        //  account for syntax sugar later.
        sealed case class Method(
          typeName: IR.Name,
          methodName: IR.Name,
          body: Expression,
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Definition
            with IRKind.Primitive {
          override protected var id: Identifier = _

          /** Creates a copy of `this`.
            *
            * @param typeName the name of the atom that the method is being
            *                 defined for
            * @param methodName the name of the method being defined on `typename`
            * @param body the body of the method
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            typeName: IR.Name                    = typeName,
            methodName: IR.Name                  = methodName,
            body: Expression                     = body,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Method = {
            val res = Method(
              typeName,
              methodName,
              body,
              location,
              passData,
              diagnostics
            )
            res.id = id
            res
          }

          override def mapExpressions(fn: Expression => Expression): Method = {
            copy(
              typeName   = typeName.mapExpressions(fn),
              methodName = methodName.mapExpressions(fn),
              body       = fn(body)
            )
          }

          override def toString: String =
            s"""
            |IR.Module.Scope.Definition.Method(
            |typeName = $typeName,
            |methodName = $methodName,
            |body = $body,
            |location = $location,
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List(typeName, methodName, body)
        }
      }
    }
  }

  // === Expression ===========================================================

  /** Enso expressions. */
  sealed trait Expression extends IR {

    /** Performs a recursive traversal of the IR, potentially transforming it.
      *
      * @param fn the function to apply across the IR
      * @return the IR, potentially transformed
      */
    def transformExpressions(
      fn: PartialFunction[Expression, Expression]
    ): Expression = {
      if (fn.isDefinedAt(this)) {
        fn(this)
      } else {
        mapExpressions(_.transformExpressions(fn))
      }
    }

    override def mapExpressions(fn: Expression => Expression): Expression
  }
  object Expression {

    // TODO [AA] Remove suspended blocks from Enso.
    /** A block expression.
      *
      * @param expressions the expressions in the block
      * @param returnValue the final expression in the block
      * @param location the source location that the node corresponds to
      * @param suspended whether or not the block is suspended
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Block(
      expressions: List[Expression],
      returnValue: Expression,
      override val location: Option[IdentifiedLocation],
      suspended: Boolean                          = false,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Expression
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param expressions the expressions in the block
        * @param returnValue the final expression in the block
        * @param location the source location that the node corresponds to
        * @param suspended whether or not the block is suspended
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        expressions: List[Expression]        = expressions,
        returnValue: Expression              = returnValue,
        location: Option[IdentifiedLocation] = location,
        suspended: Boolean                   = suspended,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Block = {
        val res = Block(
          expressions,
          returnValue,
          location,
          suspended,
          passData,
          diagnostics
        )
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Block = {
        copy(
          expressions = expressions.map(fn),
          returnValue = fn(returnValue)
        )
      }

      override def toString: String =
        s"""
        |IR.Expression.Block(
        |expressions = $expressions,
        |returnValue = $returnValue,
        |location = $location,
        |suspended = $suspended,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = expressions :+ returnValue
    }

    /** A binding expression of the form `name = expr`
      *
      * To create a binding that binds no available name, set the name of the
      * binding to an [[IR.Name.Blank]] (e.g. _ = foo a b).
      *
      * @param name the name being bound to
      * @param expression the expression being bound to `name`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Binding(
      name: IR.Name,
      expression: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Expression
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name being bound to
        * @param expression the expression being bound to `name`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: IR.Name                        = name,
        expression: Expression               = expression,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Binding = {
        val res = Binding(name, expression, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Binding = {
        copy(name = name.mapExpressions(fn), expression = fn(expression))
      }

      override def toString: String =
        s"""
        |IR.Expression.Binding(
        |name = $name,
        |expression = $expression,
        |location = $location
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(name, expression)
    }
  }

  // === Literals =============================================================

  /** Enso literals. */
  sealed trait Literal extends Expression with IRKind.Primitive {
    override def mapExpressions(fn: Expression => Expression): Literal
  }
  object Literal {

    /** A numeric Enso literal.
      *
      * @param value the textual representation of the numeric literal
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Number(
      value: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Literal {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param value the textual representation of the numeric literal
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        value: String                        = value,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Number = {
        val res = Number(value, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Number = this

      override def toString: String =
        s"""IR.Literal.Number(
        |value = $value,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()
    }

    /** A textual Enso literal.
      *
      * @param text the text of the literal
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Text(
      text: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Literal {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param text the text of the literal
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        text: String                         = text,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Text = {
        val res = Text(text, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Text = this

      override def toString: String =
        s"""
        |IR.Literal.String(
        |text = $text,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()
    }
  }

  // === Names ================================================================

  /** Enso names. */
  sealed trait Name extends Expression with IRKind.Primitive {
    val name: String

    override def mapExpressions(fn: Expression => Expression): Name
  }
  object Name {

    /** Represents occurrences of blank (`_`) expressions.
      *
      * @param location the soure location that the node corresponds to.
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Blank(
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name
        with IRKind.Sugar {
      override val name: String             = "_"
      override protected var id: Identifier = randomId

      def copy(
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Blank = {
        val res = Blank(location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Blank =
        this

      override def toString: String =
        s"""
           |IR.Expression.Blank(
           |location = $location,
           |passData = $passData,
           |diagnostics = $diagnostics,
           |id = $id
           |)
           |""".stripMargin

      override def children: List[IR] = List()
    }

    /** The representation of a literal name.
      *
      * @param name the literal text of the name
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Literal(
      override val name: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the literal text of the name
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: String                         = name,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Literal = {
        val res = Literal(name, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Literal = this

      override def toString: String =
        s"""
        |IR.Name.Literal(
        |name = $name,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()
    }

    /** A representation of the name `this`, used to refer to the current type.
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class This(
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name {
      override protected var id: Identifier = randomId
      override val name: String             = "this"

      /** Creates a copy of `this`.
        *
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): This = {
        val res = This(location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): This = this

      override def toString: String =
        s"""
        |IR.Name.This(
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()
    }

    /** A representation of the name `here`, used to refer to the current
      * module.
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Here(
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name {
      override protected var id: Identifier = randomId
      override val name: String             = "here"

      /** Creates a copy of `this`.
        *
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Here = {
        val res = Here(location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Here = this

      override def toString: String =
        s"""IR.Name.Here(
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()
    }
  }

  // === Typing ===============================================================

  /** Constructs that operate on types. */
  sealed trait Type extends Expression {
    override def mapExpressions(fn: Expression => Expression): Type
  }
  object Type {

    /** Static information about the type operators. */
    sealed trait Info {
      val name: String
    }

    /** The ascription of a type to a value.
      *
      * @param typed the expression being ascribed a type
      * @param signature the signature being ascribed to `typed`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Ascription(
      typed: Expression,
      signature: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Type
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param typed the expression being ascribed a type
        * @param signature the signature being ascribed to `typed`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        typed: Expression                    = typed,
        signature: Expression                = signature,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Ascription = {
        val res = Ascription(typed, signature, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Ascription = {
        copy(typed = fn(typed), signature = fn(signature))
      }

      override def toString: String =
        s"""IR.Type.Ascription(
        |typed = $typed,
        |signature = $signature,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(typed, signature)
    }
    object Ascription extends Info {
      override val name: String = ":"
    }

    /** A representation of the `in` portion of a type signature that represents
      * the ascription of a monadic context.
      *
      * @param typed the type being ascribed a monadic context
      * @param context the context being ascribed to `typed`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Context(
      typed: Expression,
      context: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Type
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates ac opy of `this`.
        *
        * @param typed the type being ascribed a monadic context
        * @param context the context being ascribed to `typed`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        typed: Expression                    = typed,
        context: Expression                  = context,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Context = {
        val res = Context(typed, context, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Context = {
        copy(typed = fn(typed), context = fn(context))
      }

      override def toString: String =
        s"""IR.Type.Context(
        |typed = $typed,
        |context = $context,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(typed, context)

    }
    object Context extends Info {
      override val name: String = "in"
    }

    /** IR nodes for dealing with typesets. */
    sealed trait Set extends Type {
      override def mapExpressions(fn: Expression => Expression): Set
    }
    object Set {

      /** The representation of a typeset member.
        *
        * @param label the member's label, if given
        * @param memberType the member's type, if given
        * @param value the member's value, if given
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Member(
        label: Name,
        memberType: Expression,
        value: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param label the member's label, if given
          * @param memberType the member's type, if given
          * @param value the member's value, if given
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          label: Name                          = label,
          memberType: Expression               = memberType,
          value: Expression                    = value,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Member = {
          val res =
            Member(label, memberType, value, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(fn: Expression => Expression): Member = {
          copy(
            label      = label.mapExpressions(fn),
            memberType = fn(memberType),
            value      = fn(value)
          )
        }

        override def toString: String =
          s"""
          |IR.Type.Set.Member(
          |label = $label,
          |memberType = $memberType,
          |value = $value,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |)
          |""".toSingleLine

        override def children: List[IR] = List(label, memberType, value)

      }
      object Member extends Info {
        override val name: String = "_ : _ = _"
      }

      /** The typeset subsumption judgement `<:`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Subsumption(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Subsumption = {
          val res = Subsumption(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): Subsumption = {
          copy(left = fn(left), right = fn(right))
        }

        override def toString: String =
          s"""
          |IR.Type.Set.Subsumption(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        override def children: List[IR] = List(left, right)

      }
      object Subsumption extends Info {
        override val name: String = "<:"
      }

      /** The typeset equality judgement `~`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Equality(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Equality = {
          val res = Equality(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(fn: Expression => Expression): Equality = {
          copy(left = fn(left), right = fn(right))
        }

        override def toString: String =
          s"""
          |IR.Type.Set.Equality(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        override def children: List[IR] = List(left, right)

      }
      object Equality extends Info {
        override val name: String = "~"
      }

      /** The typeset concatenation operator `,`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Concat(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Concat = {
          val res = Concat(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(fn: Expression => Expression): Concat = {
          copy(left = fn(left), right = fn(right))
        }

        override def toString: String =
          s"""
          |IR.Type.Set.Concat(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        override def children: List[IR] = List(left, right)

      }
      object Concat extends Info {
        override val name: String = ","
      }

      /** The typeset union operator `|`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Union(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Union = {
          val res = Union(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(fn: Expression => Expression): Union = {
          copy(left = fn(left), right = fn(right))
        }

        override def toString: String =
          s"""
          |IR.Type.Set.Union(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        override def children: List[IR] = List(left, right)

      }
      object Union extends Info {
        override val name: String = "|"
      }

      /** The typeset intersection operator `&`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Intersection(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Intersection = {
          val res = Intersection(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): Intersection = {
          copy(left = fn(left), right = fn(right))
        }

        override def toString: String =
          s"""
          |IR.Type.Set.Intersection(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        override def children: List[IR] = List(left, right)

      }
      object Intersection extends Info {
        override val name: String = "&"
      }

      /** The typeset subtraction operator `\`.
        *
        * @param left the left operand
        * @param right the right operand
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Subtraction(
        left: Expression,
        right: Expression,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Set
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand
          * @param right the right operand
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: Expression                     = left,
          right: Expression                    = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Subtraction = {
          val res = Subtraction(left, right, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): Subtraction = {
          copy(left = fn(left), right = fn(right))
        }

        override def toString: String =
          s"""
          |IR.Type.Set.Subtraction(
          |left = $left,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |""".toSingleLine

        override def children: List[IR] = List(left, right)

      }
      object Subtraction extends Info {
        override val name: String = "\\"
      }
    }
  }

  // === Function =============================================================

  /** Functions in Enso. */
  sealed trait Function extends Expression {

    /** The function arguments.
      *
      * Please note that while the source language does not represent
      * multi-argument lambdas, the internal language can and does.
      */
    val arguments: List[DefinitionArgument]

    /** The body of the function */
    val body: Expression

    /** Whether or not the function _can_ be tail-call optimised.
      *
      * Please note that this being set to `true` does not _guarantee_ that the
      * function is optimised.
      */
    val canBeTCO: Boolean

    override def mapExpressions(fn: Expression => Expression): Function
  }
  object Function {

    /** The primitive function type in Enso: `->`.
      *
      * It should be noted that while the _surface_ language does not support
      * multi-argument lambdas, our internal representation does so to allow for
      * better optimisation.
      *
      * @param arguments the arguments to the lambda
      * @param body the body of the lambda
      * @param location the source location that the node corresponds to
      * @param canBeTCO whether or not the function can be tail-call optimised
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Lambda(
      override val arguments: List[DefinitionArgument],
      override val body: Expression,
      override val location: Option[IdentifiedLocation],
      override val canBeTCO: Boolean              = true,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Function
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param arguments the arguments to the lambda
        * @param body the body of the lambda
        * @param location the source location that the node corresponds to
        * @param canBeTCO whether or not the function can be tail-call optimised
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        arguments: List[DefinitionArgument]  = arguments,
        body: Expression                     = body,
        location: Option[IdentifiedLocation] = location,
        canBeTCO: Boolean                    = canBeTCO,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Lambda = {
        val res =
          Lambda(arguments, body, location, canBeTCO, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Lambda = {
        copy(arguments = arguments.map(_.mapExpressions(fn)), body = fn(body))
      }

      override def toString: String =
        s"""
        |IR.Function.Lambda(
        |arguments = $arguments,
        |body = $body,
        |location = $location,
        |canBeTCO = $canBeTCO,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = arguments :+ body

    }
  }

  // === Definition-Site Arguments ============================================

  /** Definition-site arguments in Enso. */
  sealed trait DefinitionArgument extends IR {

    /** The name of the argument. */
    val name: IR.Name

    /** The default value of the argument. */
    val defaultValue: Option[Expression]

    /** Whether or not the argument is suspended. */
    val suspended: Boolean

    override def mapExpressions(
      fn: Expression => Expression
    ): DefinitionArgument
  }
  object DefinitionArgument {

    /** The representation of an argument from a [[Function]] or
      * [[IR.Module.Scope.Definition.Atom]] definition site.
      *
      * To create an ignored argument, the argument name should be an
      * [[IR.Name.Blank]].
      *
      * @param name the name of the argument
      * @param defaultValue the default value of the argument, if present
      * @param suspended whether or not the argument has its execution suspended
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Specified(
      override val name: IR.Name,
      override val defaultValue: Option[Expression],
      override val suspended: Boolean,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends DefinitionArgument
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name of the argument
        * @param defaultValue the default value of the argument, if present
        * @param suspended whether or not the argument has its execution suspended
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: IR.Name                        = name,
        defaultValue: Option[Expression]     = defaultValue,
        suspended: Boolean                   = suspended,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Specified = {
        val res = Specified(
          name,
          defaultValue,
          suspended,
          location,
          passData,
          diagnostics
        )
        res.id = id
        res
      }

      def mapExpressions(fn: Expression => Expression): Specified = {
        copy(
          name         = name.mapExpressions(fn),
          defaultValue = defaultValue.map(fn)
        )
      }

      override def toString: String =
        s"""
        |IR.DefinitionArgument.Specified(
        |name = $name,
        |defaultValue = $defaultValue,
        |suspended = $suspended,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = name :: defaultValue.toList
    }
  }

  // === Applications =========================================================

  /** All function applications in Enso. */
  sealed trait Application extends Expression {
    override def mapExpressions(fn: Expression => Expression): Application
  }
  object Application {

    /** A standard prefix function application.
      *
      * @param function the function being called
      * @param arguments the arguments to the function being called
      * @param hasDefaultsSuspended whether the function application has any
      *                             argument defaults in `function` suspended
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Prefix(
      function: Expression,
      arguments: List[CallArgument],
      hasDefaultsSuspended: Boolean,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Application
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param function the function being called
        * @param arguments the arguments to the function being called
        * @param hasDefaultsSuspended whether the function application has any
        *                             argument defaults in `function` suspended
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        function: Expression                 = function,
        arguments: List[CallArgument]        = arguments,
        hasDefaultsSuspended: Boolean        = hasDefaultsSuspended,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Prefix = {
        val res =
          Prefix(
            function,
            arguments,
            hasDefaultsSuspended,
            location,
            passData,
            diagnostics
          )
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Prefix = {
        copy(function = fn(function), arguments.map(_.mapExpressions(fn)))
      }

      override def toString: String =
        s"""
        |IR.Application.Prefix(
        |function = $function,
        |arguments = $arguments,
        |hasDefaultsSuspended = $hasDefaultsSuspended,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = function :: arguments

    }

    /** A representation of a term that is explicitly forced.
      *
      * @param target the expression being forced
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Force(
      target: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Application
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param target the expression being forced
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        target: Expression                   = target,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Force = {
        val res = Force(target, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Force = {
        copy(target = fn(target))
      }

      override def toString: String =
        s"""
        |IR.Application.Force(
        |target = $target,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(target)

    }

    /** Literal applications in Enso. */
    sealed trait Literal extends Application {
      override def mapExpressions(fn: Expression => Expression): Literal
    }

    object Literal {

      /** A representation of a vector literal.
        *
        * @param items the items being put in the vector
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Sequence(
        items: List[Expression],
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Literal
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        override def mapExpressions(fn: Expression => Expression): Sequence =
          copy(items = items.map(fn))

        /** Creates a copy of `this`.
          *
          * @param items the items held by this vector
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          items: List[Expression]              = items,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Sequence = {
          val res = Sequence(items, location, passData, diagnostics)
          res.id = id
          res
        }

        override def toString: String =
          s"""
          |IR.Application.Literal.Vector(
          |text = $items,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |)
          |""".toSingleLine

        override def children: List[IR] = items
      }
    }

    /** Operator applications in Enso. */
    sealed trait Operator extends Application {
      override def mapExpressions(fn: Expression => Expression): Operator
    }
    object Operator {

      /** A representation of a generic binary operator application in Enso.
        *
        * @param left the left operand to `operator`
        * @param operator the operator function being called
        * @param right the right operand to `operator`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Binary(
        left: CallArgument,
        operator: IR.Name,
        right: CallArgument,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Operator
          with IRKind.Sugar {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param left the left operand to `operator`
          * @param operator the operator function being called
          * @param right the right operand to `operator`
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          left: CallArgument                   = left,
          operator: IR.Name                    = operator,
          right: CallArgument                  = right,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Binary = {
          val res =
            Binary(left, operator, right, location, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(fn: Expression => Expression): Binary = {
          copy(left = left.mapExpressions(fn), right = right.mapExpressions(fn))
        }

        override def toString: String =
          s"""
          |IR.Application.Operator.Binary(
          |left = $left,
          |operator = $operator,
          |right = $right,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |)
          |""".toSingleLine

        override def children: List[IR] = List(left, operator, right)

      }

      /** Operator sections. */
      sealed trait Section extends Operator {
        override def mapExpressions(fn: Expression => Expression): Section
      }
      object Section {

        /** Represents a left operator section of the form `(arg op)`.
          *
          * @param arg the argument (on the left of the operator)
          * @param operator the operator
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Left(
          arg: CallArgument,
          operator: IR.Name,
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Section
            with IRKind.Sugar {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param arg the argument (on the left of the operator)
            * @param operator the operator
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            arg: CallArgument                    = arg,
            operator: IR.Name                    = operator,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: IR.Identifier                    = id
          ): Left = {
            val res = Left(arg, operator, location, passData, diagnostics)
            res.id = id
            res
          }

          override def mapExpressions(fn: Expression => Expression): Section =
            copy(
              arg      = arg.mapExpressions(fn),
              operator = operator.mapExpressions(fn)
            )

          override def toString: String =
            s"""
            |IR.Application.Operator.Section.Left(
            |arg = $arg,
            |operator =  $operator,
            |location = $location,
            |passData = $passData,
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List(arg, operator)
        }

        /** Represents a sides operator section of the form `(op)`
          *
          * @param operator the operator
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Sides(
          operator: IR.Name,
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Section
            with IRKind.Sugar {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param operator the operator
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            operator: IR.Name                    = operator,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Sides = {
            val res = Sides(operator, location, passData, diagnostics)
            res.id = id
            res
          }

          override def mapExpressions(fn: Expression => Expression): Section =
            copy(operator = operator.mapExpressions(fn))

          override def toString: String =
            s"""
            |IR.Application.Operator.Section.Centre(
            |operator =  $operator,
            |location = $location,
            |passData = $passData,
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List(operator)
        }

        /** Represents a right operator section of the form `(op arg)`
          *
          * @param operator the operator
          * @param arg the argument (on the right of the operator)
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Right(
          operator: IR.Name,
          arg: CallArgument,
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Section
            with IRKind.Sugar {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param operator the operator
            * @param arg the argument (on the right of the operator)
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            operator: IR.Name                    = operator,
            arg: CallArgument                    = arg,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Right = {
            val res = Right(operator, arg, location, passData, diagnostics)
            res.id = id
            res
          }

          override def mapExpressions(fn: Expression => Expression): Section = {
            copy(
              operator = operator.mapExpressions(fn),
              arg      = arg.mapExpressions(fn)
            )
          }

          override def toString: String =
            s"""
            |IR.Application.Operator.Section.Right(
            |operator =  $operator,
            |arg = $arg,
            |location = $location,
            |passData = $passData,
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List(operator, arg)
        }
      }
    }
  }

  // === Call-Site Arguments ==================================================

  /** Call-site arguments in Enso. */
  sealed trait CallArgument extends IR {

    /** The name of the argument, if present. */
    val name: Option[IR.Name]

    /** The expression of the argument, if present. */
    val value: Expression

    /** Whether or not the argument should be suspended at code generation time.
      *
      * A value of `Some(true)` implies that code generation should suspend the
      * argument. A value of `Some(false)` implies that code generation should
      * _not_ suspend the argument. A value of [[None]] implies that the
      * information is missing.
      */
    val shouldBeSuspended: Option[Boolean]

    override def mapExpressions(fn: Expression => Expression): CallArgument
  }
  object CallArgument {

    /** A representation of an argument at a function call site.
      *
      * A [[CallArgument]] where the `value` is an [[IR.Name.Blank]] is a
      * representation of a lambda shorthand argument.
      *
      * @param name the name of the argument being called, if present
      * @param value the expression being passed as the argument's value
      * @param location the source location that the node corresponds to
      * @param shouldBeSuspended whether or not the argument should be passed
      *        suspended
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Specified(
      override val name: Option[IR.Name],
      override val value: Expression,
      override val location: Option[IdentifiedLocation],
      override val shouldBeSuspended: Option[Boolean] = None,
      override val passData: MetadataStorage          = MetadataStorage(),
      override val diagnostics: DiagnosticStorage     = DiagnosticStorage()
    ) extends CallArgument
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name of the argument being called, if present
        * @param value the expression being passed as the argument's value
        * @param location the source location that the node corresponds to
        * @param shouldBeSuspended whether or not the argument should be passed
        *        suspended
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: Option[IR.Name]                = name,
        value: Expression                    = value,
        location: Option[IdentifiedLocation] = location,
        shouldBeSuspended: Option[Boolean]   = shouldBeSuspended,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Specified = {
        val res = Specified(
          name,
          value,
          location,
          shouldBeSuspended,
          passData,
          diagnostics
        )
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Specified = {
        copy(name = name.map(n => n.mapExpressions(fn)), value = fn(value))
      }

      override def toString: String =
        s"""
        |IR.CallArgument.Specified(
        |name = $name,
        |value = $value,
        |location = $location,
        |shouldBeSuspended = $shouldBeSuspended,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = name.toList :+ value
    }
  }

  // === Case Expression ======================================================

  /** The Enso case expression. */
  sealed trait Case extends Expression {
    override def mapExpressions(fn: Expression => Expression): Case
  }
  object Case {

    /** The main body of the Enso case expression.
      *
      * @param scrutinee the expression whose value is being matched on
      * @param branches the branches of the case expression
      * @param fallback a fallback branch, if provided explicitly
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Expr(
      scrutinee: Expression,
      branches: Seq[Branch],
      fallback: Option[Expression],
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Case
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param scrutinee the expression whose value is being matched on
        * @param branches the branches of the case expression
        * @param fallback a fallback branch, if provided explicitly
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        scrutinee: Expression                = scrutinee,
        branches: Seq[Branch]                = branches,
        fallback: Option[Expression]         = fallback,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Expr = {
        val res =
          Expr(scrutinee, branches, fallback, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Expr = {
        copy(
          scrutinee = fn(scrutinee),
          branches.map(_.mapExpressions(fn)),
          fallback.map(fn)
        )
      }

      override def toString: String =
        s"""
        |IR.Case.Expr(
        |scutinee = $scrutinee,
        |branches = $branches,
        |fallback = $fallback,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] =
        scrutinee :: branches.toList ++ fallback.toList

    }

    /** A branch in a case statement.
      *
      * @param pattern the pattern that attempts to match against the scrutinee
      * @param expression the expression that is executed if the pattern matches
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Branch(
      pattern: Expression,
      expression: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Case
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param pattern the pattern that attempts to match against the scrutinee
        * @param expression the expression that is executed if the pattern matches
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        pattern: Expression                  = pattern,
        expression: Expression               = expression,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Branch = {
        val res = Branch(pattern, expression, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Branch = {
        copy(pattern = fn(pattern), expression = fn(expression))
      }

      override def toString: String =
        s"""
        |IR.Case.Branch(
        |pattern = $pattern,
        |expression = $expression,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(pattern, expression)

    }

    /** The different types of patterns that can occur in a match. */
    sealed trait Pattern extends IR {
      override def mapExpressions(fn: Expression => Expression): Pattern
    }
    object Pattern {
      // TODO [AA] Better differentiate the types of patterns that can occur
    }
  }

  // === Comments =============================================================

  /** Enso comment entities. */
  sealed trait Comment extends Expression {
    override def mapExpressions(fn: Expression => Expression): Comment

    /** The expression being commented. */
    val commented: Expression
  }
  object Comment {

    /** A documentation comment in the Enso source.
      *
      * @param commented the expression with which the comment is associated
      * @param doc the documentation of `commented`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Documentation(
      override val commented: Expression,
      doc: Doc,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Comment
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param commented the expression with which the comment is associated
        * @param doc the documentation of `commented`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        commented: Expression                = commented,
        doc: Doc                             = doc,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Documentation = {
        val res = Documentation(commented, doc, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(
        fn: Expression => Expression
      ): Documentation = {
        copy(commented = fn(commented))
      }

      override def toString: String =
        s"""
        |IR.Comment.Documentation(
        |commented = $commented,
        |doc = $doc,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(commented)

    }
  }

  // === Foreign ==============================================================

  /** Foreign code entities. */
  sealed trait Foreign extends Expression {
    override def mapExpressions(fn: Expression => Expression): Foreign
  }
  object Foreign {

    /** A foreign code definition in Enso.
      *
      * @param lang the foreign language being written
      * @param code the code written in `lang`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Definition(
      lang: String,
      code: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Foreign
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param lang the foreign language being written
        * @param code the code written in `lang`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        lang: String                         = lang,
        code: String                         = code,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Definition = {
        val res = Definition(lang, code, location, passData, diagnostics)
        res.id = id
        res
      }

      override def mapExpressions(fn: Expression => Expression): Definition =
        this

      override def toString: String =
        s"""
        |IR.Foreign.Definition(
        |lang = $lang,
        |code = $code,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()

    }
  }

  // === Diagnostics ==========================================================

  /** A representation of various kinds of diagnostic in the IR. */
  sealed trait Diagnostic {

    /**
      * @return a human-readable description of this error condition.
      */
    def message: String

    /** The location at which the diagnostic occurs. */
    val location: Option[IdentifiedLocation]
  }
  object Diagnostic {

    /** Represents the various kinds of errors in the IR. */
    sealed trait Kind
    object Kind {

      /** Errors that should be reported during the static compilation phase of
        * execution.
        */
      sealed trait Static extends Kind

      /** Errors that should remain at runtime for display during interactive
        * execution.
        */
      sealed trait Interactive extends Kind
    }
  }

  // === Warnings =============================================================

  /** A trait for all warnings in Enso's IR. */
  sealed trait Warning extends Diagnostic
  object Warning {

    /** Warnings about unused language entities. */
    sealed trait Unused extends Warning {
      val name: IR.Name
    }
    object Unused {

      /** A warning about an unused function argument.
        *
        * @param name the name that is unused
        */
      sealed case class FunctionArgument(override val name: Name)
          extends Unused {
        override def message: String = s"Unused function argument ${name.name}."

        override def toString: String = s"Unused.FunctionArgument(${name.name})"

        override val location: Option[IdentifiedLocation] = name.location
      }

      /** A warning about an unused binding.
        *
        * @param name the name that is unused
        */
      sealed case class Binding(override val name: Name) extends Unused {
        override def message: String = s"Unused variable ${name.name}."

        override def toString: String = s"Unused.Binding(${name.name})"

        override val location: Option[IdentifiedLocation] = name.location
      }
    }

    /** Warnings about shadowing names. */
    sealed trait Shadowed extends Warning {

      /** The [[IR]] shadowing the warned expression. */
      val shadower: IR
    }
    object Shadowed {

      /** A warning that a later-defined lambda parameter shadows an
        * earlier-defined lambda parameter.
        *
        * @param shadowedName the name being shadowed
        * @param shadower the expression shadowing `warnedExpr`
        */
      sealed case class FunctionParam(
        shadowedName: String,
        override val shadower: IR,
        override val location: Option[IdentifiedLocation]
      ) extends Shadowed {

        override def message: String =
          s"The argument $shadowedName is shadowed by $shadower."
      }
    }
  }

  // === Errors ===============================================================

  /** A trait for all errors in Enso's IR. */
  sealed trait Error extends Expression with Diagnostic {
    override def mapExpressions(fn: Expression => Expression): Error
  }
  object Error {

    /** A representation of an Enso syntax error.
      *
      * @param ast the erroneous AST
      * @param reason the cause of this error
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Syntax(
      ast: AST,
      reason: Syntax.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param ast the erroneous AST
        * @param reason the cause of this error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        ast: AST                       = ast,
        reason: Syntax.Reason          = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): Syntax = {
        val res = Syntax(ast, reason, passData, diagnostics)
        res.id = id
        res
      }

      override val location: Option[IdentifiedLocation] =
        ast.location.map(IdentifiedLocation(_, ast.id))

      override def mapExpressions(fn: Expression => Expression): Syntax = this

      override def toString: String =
        s"""
        |IR.Error.Syntax(
        |ast = $ast,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()

      override def message: String = reason.explanation
    }

    object Syntax {

      /**
        * A common type for all syntax errors expected by the language.
        */
      sealed trait Reason {

        /**
          * @return a human-readable description of the error.
          */
        def explanation: String
      }

      case class UnsupportedSyntax(syntaxName: String) extends Reason {
        override def explanation: String =
          s"Syntax is not supported yet: $syntaxName."
      }

      case object EmptyParentheses extends Reason {
        override def explanation: String = "Parentheses can't be empty."
      }

      case object UnexpectedExpression extends Reason {
        override def explanation: String = "Unexpected expression."
      }

      case object UnrecognizedToken extends Reason {
        override def explanation: String = "Unrecognized token."
      }

      case object InvalidSuffix extends Reason {
        override def explanation: String = "Invalid suffix."
      }

      case object UnclosedTextLiteral extends Reason {
        override def explanation: String = "Unclosed text literal."
      }

      case object NamedArgInSection extends Reason {
        override def explanation: String = "Named argument in operator section."
      }

      case object NamedArgInOperator extends Reason {
        override def explanation: String = "Named argument in operator section."
      }
    }

    /** A representation of an invalid piece of IR.
      *
      * @param ir the IR that is invalid
      * @param passData any annotations from compiler passes
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class InvalidIR(
      ir: IR,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Static
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param ir the IR that is invalid
        * @param passData any annotations from compiler passes
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        ir: IR                         = ir,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): InvalidIR = {
        val res = InvalidIR(ir, passData, diagnostics)
        res.id = id
        res
      }

      override val location: Option[IdentifiedLocation] = ir.location

      override def mapExpressions(fn: Expression => Expression): InvalidIR =
        this

      override def toString: String =
        s"""
        |IR.Error.InvalidIR(
        |ir = $ir,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(ir)

      override def message: String =
        "InvalidIR: Please report this as a compiler bug."

    }

    /** Errors pertaining to the redefinition of language constructs that are
      * not allowed to be.
      */
    sealed trait Redefined extends Error
    object Redefined {

      /** An error representing the redefinition of a method in a given module.
        * This is also known as a method overload.
        *
        * @param atomName the name of the atom the method was being redefined on
        * @param methodName the method name being redefined on `atomName`
        * @param location the location in the source to which this error
        *                 corresponds
        * @param passData the pass metadata for the error
        * @param diagnostics any diagnostics associated with this error.
        */
      sealed case class Method(
        atomName: IR.Name,
        methodName: IR.Name,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with Module.Scope.Definition
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param atomName the name of the atom the method was being redefined on
          * @param methodName the method name being redefined on `atomName`
          * @param location the location in the source to which this error
          *                 corresponds
          * @param passData the pass metadata for the error
          * @param diagnostics any diagnostics associated with this error.
          * @param id the identifier for the node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          atomName: IR.Name                    = atomName,
          methodName: IR.Name                  = methodName,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Method = {
          val res =
            Method(atomName, methodName, location, passData, diagnostics)
          res.id = id
          res
        }

        override def message: String =
          s"Method overloads are not supported: ${atomName.name}." +
          s"${methodName.name} is defined multiple times in this module."

        override def mapExpressions(fn: Expression => Expression): Method = this

        override def toString: String =
          s"""
             |IR.Error.Redefined.Method(
             |atomName = $atomName,
             |methodName = $methodName,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        override def children: List[IR] = List(atomName, methodName)
      }

      /** An error representing the redefinition of an atom in a given module.
        *
        * @param atomName the name of the atom being redefined
        * @param location the location in the source to which this error
        *                 corresponds
        * @param passData the pass metadata for the error
        * @param diagnostics any diagnostics associated with this error.
        */
      sealed case class Atom(
        atomName: IR.Name,
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with Module.Scope.Definition
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param atomName the name of the atom the method was being redefined
          *                 on
          * @param location the location in the source to which this error
          *                 corresponds
          * @param passData the pass metadata for the error
          * @param diagnostics any diagnostics associated with this error.
          * @param id the identifier for the node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          atomName: IR.Name                    = atomName,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Atom = {
          val res =
            Atom(atomName, location, passData, diagnostics)
          res.id = id
          res
        }

        override def message: String =
          s"Redefining atoms is not supported: ${atomName.name} is " +
          s"defined multiple times in this module."

        override def mapExpressions(fn: Expression => Expression): Atom = this

        override def toString: String =
          s"""
             |IR.Error.Redefined.Atom(
             |atomName = $atomName,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        override def children: List[IR] = List(atomName)
      }

      /** An error representing the redefinition of a binding in a given scope.
        *
        * While bindings in child scopes are allowed to _shadow_ bindings in
        * parent scopes, a binding cannot be redefined within a given scope.
        *
        * @param invalidBinding the invalid binding
        * @param passData the pass metadata for the error
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Binding(
        invalidBinding: IR.Expression.Binding,
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param invalidBinding the invalid binding
          * @param passData the pass metadata for the error
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          invalidBinding: IR.Expression.Binding = invalidBinding,
          passData: MetadataStorage             = passData,
          diagnostics: DiagnosticStorage        = diagnostics,
          id: Identifier                        = id
        ): Binding = {
          val res = Binding(invalidBinding, passData, diagnostics)
          res.id = id
          res
        }

        override val location: Option[IdentifiedLocation] =
          invalidBinding.location

        override def mapExpressions(fn: Expression => Expression): Binding =
          this

        override def toString: String =
          s"""
             |IR.Error.Redefined.Binding(
             |invalidBinding = $invalidBinding,
             |location = $location,
             |passData = ${this.showPassData},
             |diagnostics = $diagnostics,
             |id = $id
             |)
             |""".stripMargin

        override def children: List[IR] = List(invalidBinding)

        override def message: String =
          s"Variable ${invalidBinding.name.name} is being redefined."

      }
    }
  }

  // ==========================================================================
  // === Primitive / Sugar ====================================================
  // ==========================================================================

  /** A trait representing the classification of IR nodes into either primitive
    * (constructs which will remain after desugaring) or sugar (constructs that
    * should be removed by the desugaring passes).
    */
  sealed trait IRKind
  object IRKind {

    /** This trait encodes that a given piece of the [[IR]] is considered to be
      * a primitive construct in Enso.
      */
    sealed trait Primitive extends IRKind

    /** This trait encodes that a given piece of the [[IR]] is considered to
      * represent syntax sugar in Enso.
      *
      * All [[Sugar]] constructs should be desugared into [[Primitive]]
      * constructs as soon as possible.
      */
    sealed trait Sugar extends IRKind

    /** This trait encodes that a given piece of [[IR]] is used to represent an
      * optimisation on the IR in Enso.
      */
    sealed trait Optimisation extends IRKind
  }

  // ==========================================================================
  // === Extension Methods ====================================================
  // ==========================================================================

  /** This class adds an extension method to control how the pass data element
    * of the IR is printed.
    *
    * @param ir the IR to print the pass data for
    */
  implicit class ShowPassData(ir: IR) {

    /** Creates a string representation of the pass data for a given IR node.
      *
      * @return a string representation of the pass data for [[ir]]
      */
    def showPassData: String = {
      val metaString = ir.passData.map((p, m) => (p, m.metadataName)).values

      s"$metaString"
    }
  }

  /** Adds extension methods on strings to aid in writing custom to string
    * overrides.
    *
    * @param string the string to process
    */
  implicit class ToStringHelper(string: String) {

    /** Converts a multiline string to a single line
      *
      * @return [[string]], converted to a single line
      */
    def toSingleLine: String = {
      val lines = string.stripMargin.split("\n").toList.filterNot(_ == "")

      val body = lines.tail.dropRight(1).mkString(" ")

      s"${lines.head}$body${lines.last}"
    }
  }

  // ==========================================================================
  // === Diagnostics Storage ==================================================
  // ==========================================================================

  /** Storage for diagnostics in IR nodes.
    *
    * @param initDiagnostics the initial diagnostics
    */
  sealed class DiagnosticStorage(initDiagnostics: Seq[Diagnostic] = Seq()) {
    private var diagnostics: List[Diagnostic] = initDiagnostics.toList

    /** Adds a new diagnostic to the storage
      *
      * @param diagnostic the new diagnostic to store
      */
    def add(diagnostic: Diagnostic): Unit = {
      diagnostics = diagnostic :: diagnostics
    }

    /** Adds new diagnostics to the storage.
      *
      * @param newDiagnostics the new diagnostics to store
      */
    def add(newDiagnostics: Seq[Diagnostic]): Unit = {
      diagnostics = newDiagnostics.toList ::: diagnostics
    }

    /** Applies the function `f` across the diagnostic storage, producing a
      * result sequence.
      *
      * @param f the function to apply
      * @tparam R the result type of `f`
      * @return the sequence that results from applying `f` over the storage
      */
    def map[R](f: IR.Diagnostic => R): Seq[R] = {
      diagnostics.map(f)
    }

    /** Applies the function `f` across the diagnostic storage in place.
      *
      * @param f the function to apply
      */
    def mapInPlace(f: IR.Diagnostic => IR.Diagnostic): Unit = {
      diagnostics = diagnostics.map(f)
    }

    /** Performs a collection operation on the diagnostics storage, producing
      * a new sequence.
      *
      * @param pf the partial function to apply
      * @tparam R the result type of the partial function
      * @return the result of collecting across the storage with `pf`
      */
    def collect[R](pf: PartialFunction[IR.Diagnostic, R]): Seq[R] = {
      diagnostics.collect(pf)
    }

    /** Filters the elements of the diagnostic storage using the predicate.
      *
      * @param pred the predicate to filter with
      * @return a new diagnostic storage instance containing elements matching
      *         `pred`
      */
    def filter(pred: IR.Diagnostic => Boolean): DiagnosticStorage = {
      new DiagnosticStorage(diagnostics.filter(pred))
    }

    /** Filters the elements of the diagnostic storage in place using the
      * predicate.
      *
      * @param pred the predicate to filter with
      */
    def filterInPlace(pred: IR.Diagnostic => Boolean): Unit = {
      diagnostics = diagnostics.filter(pred)
    }

    /** Performs a left fold over the diagnostic storage to produce a result.
      *
      * @param init the starting value
      * @param op the operator to use to fold
      * @tparam L the result type of the fold
      * @return the result of folding over the storage using `op` starting wit
      *         `init`
      */
    def foldLeft[L](init: L)(op: (L, IR.Diagnostic) => L): L = {
      diagnostics.foldLeft(init)(op)
    }

    /** Checks two diagnostics storages for equality.
      *
      * @param obj the object to check against `this`
      * @return `true` if `this == obj`, otherwise `false`
      */
    override def equals(obj: Any): Boolean = obj match {
      case that: DiagnosticStorage => this.diagnostics == that.diagnostics
      case _                       => false
    }

    /** Creates a string representation of `this` diagnostic storage.
      *
      * @return the string representation of `this`
      */
    override def toString: String =
      s"DiagnosticStorage(diagnostics = $diagnostics)"

    /** Creates a list of the diagnostics contained in the diagnostics storage.
      *
      * @return a list of the diagnostics in the storage
      */
    def toList: List[IR.Diagnostic] = {
      diagnostics
    }
  }
  object DiagnosticStorage {

    /** Creates a new instance of the diagnostics storage.
      *
      * @param initDiagnostics the initial diagnostics to construct it with
      * @return a new diagnostics storage instance
      */
    def apply(initDiagnostics: Seq[Diagnostic] = Seq()): DiagnosticStorage =
      new DiagnosticStorage(initDiagnostics)
  }

  // ==========================================================================
  // === Useful Extension Methods =============================================
  // ==========================================================================

  /** Adds extension methods for working directly with the diagnostics on the
    * IR.
    *
    * @param ir the IR to add the methods to
    * @tparam T the concrete type of the IR
    */
  implicit class AsDiagnostics[T <: IR](ir: T) {

    /** Adds a new diagnostic entity to [[IR]].
      *
      * @param diagnostic the diagnostic to add
      * @return [[ir]] with added diagnostics
      */
    def addDiagnostic(diagnostic: IR.Diagnostic): T = {
      ir.diagnostics.add(diagnostic)
      ir
    }
  }

  /** Adds extension methods for working directly with the metadata on the IR.
    *
    * @param ir the IR to add the methods to
    * @tparam T the concrete type of the IR
    */
  implicit class AsMetadata[T <: IR](ir: T) {

    /** Adds a metadata pair to the node metadata.
      *
      * This will overwrite any entry whose key matches [[MetadataPair#pass]].
      *
      * @param metadataPair the pair to add to the storage
      * @tparam K the concrete type of the pass
      */
    def updateMetadata[K <: IRPass](metadataPair: MetadataPair[K]): T = {
      ir.passData.update(metadataPair)
      ir
    }

    /** Gets the metadata for the specified pass.
      *
      * @param pass the pass to get the metadata for
      * @tparam K the concrete type of `pass`
      * @return the metadata for `pass`, if it exists
      */
    def getMetadata[K <: IRPass](pass: K): Option[pass.Metadata] = {
      ir.passData.get(pass)
    }

    /** Unsafely gets the metadata for the specified pass, if it exists.
      *
      * @param pass the pass to get metadata for
      * @param msg the message to throw with if the unsafe get fails
      * @tparam K the concrete type of `pass`
      * @throws CompilerError if no metadata exists for `pass`
      * @return the metadata for `pass`, if it exists
      */
    @throws[CompilerError]
    def unsafeGetMetadata[K <: IRPass](
      pass: IRPass,
      msg: String
    ): pass.Metadata = {
      ir.passData.getUnsafe(pass)(msg)
    }
  }
}
