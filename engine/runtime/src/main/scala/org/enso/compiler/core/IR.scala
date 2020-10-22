package org.enso.compiler.core

import java.util.UUID

import org.enso.compiler.core.IR.{Expression, IdentifiedLocation}
import org.enso.compiler.core.ir.{DiagnosticStorage, MetadataStorage}
import org.enso.compiler.core.ir.MetadataStorage.MetadataPair
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.syntax.text.{AST, Debug, Location}

import scala.annotation.unused

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
  *
  * See also: Note [IR Equality and hashing]
  */
sealed trait IR {

  /** Storage for metadata that the node has been tagged with as the result of
    * various compiler passes.
    */
  val passData: MetadataStorage

  /** The source location that the node corresponds to. */
  val location: Option[IdentifiedLocation]

  /** Sets the location for an IR node.
    *
    * @param location the new location for the IR node
    * @return the IR node with its location set to `location`
    */
  def setLocation(location: Option[IdentifiedLocation]): IR

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

  /** Creates a deep structural copy of `this`, representing the same structure
    * but with all new identifiers.
    *
    * The location, diagnostics and metadata will be empty in the duplicated
    * node.
    *
    * @param keepLocations whether or not locations should be kept in the
    *                      duplicated IR
    * @param keepMetadata whether or not the pass metadata should be kept in the
    *                      duplicated IR
    * @param keepDiagnostics whether or not the diagnostics should be kept in
    *                        the duplicated IR
    * @return a deep structural copy of `this`
    */
  def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true
  ): IR

  /** Shows the IR as code.
    *
    * @param indent the current indentation level
    * @return a string representation of `this`
    */
  def showCode(indent: Int = 0): String
}

/* Note [IR Equality and hashing]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * As the IRs are implemented as case classes, their equality is determined by
 * the values included in the constructor. These include the MetadataStorage and
 * DiagnosticStorage. These two storages break the contract of `hashCode` by
 * overriding the `equals` method to compare for equality by their contents, but
 * not `hashCode` (because it would have to be mutable). As the case classes of
 * the IR use that to implement their own equality and hashing, their
 * implementation is also troubled by this. Instances of IR that are equal by
 * the `equals` function, may still return different `hashCode`.
 *
 * The MetadataStorage and DiagnosticStorage should not be used for checking
 * equality of the IR. This should be addressed when the IR is refactored to be
 * properly mutable.
 */

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

  /** Couples a location with a possible source identifier.
    *
    * @param location the code location.
    * @param id the identifier for the location.
    */
  case class IdentifiedLocation(location: Location, id: Option[AST.ID]) {

    /** @return the character index of the start of this source location.
      */
    def start: Int = location.start

    /** @return the character index of the end of this source location.
      */
    def end: Int = location.end

    /** @return the length in characters of this location.
      */
    def length: Int = location.length
  }
  object IdentifiedLocation {

    /** Utility constructor, building a location without an ID.
      *
      * @param location the code location.
      * @return an [[IdentifiedLocation]] corresponding to the input location.
      */
    def apply(location: Location): IdentifiedLocation =
      IdentifiedLocation(location, None)
  }

  /** Generates an indent of `n` spaces.
    *
    * @param n the number of spaces
    * @return a string representing an `n`-space indent
    */
  def mkIndent(n: Int): String = {
    " " * n
  }

  /** The size of a single indentation level. */
  val indentLevel: Int = 4

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

    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Empty =
      copy(
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = randomId
      )

    override def setLocation(location: Option[IdentifiedLocation]): Empty =
      copy(location = location)

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

    override def showCode(indent: Int): String = "IR.Empty"
  }

  // === Module ===============================================================

  /** A representation of a top-level Enso module.
    *
    * Modules may only contain imports and top-level bindings, with no top-level
    * executable code.
    *
    * @param imports the import statements that bring other modules into scope
    * @param exports the export statements for this module
    * @param bindings the top-level bindings for this module
    * @param location the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Module(
    imports: List[Module.Scope.Import],
    exports: List[Module.Scope.Export],
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
      * @param exports the export statements for this module
      * @param bindings the top-level bindings for this module
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      imports: List[Module.Scope.Import]      = imports,
      exports: List[Module.Scope.Export]      = exports,
      bindings: List[Module.Scope.Definition] = bindings,
      location: Option[IdentifiedLocation]    = location,
      passData: MetadataStorage               = passData,
      diagnostics: DiagnosticStorage          = diagnostics,
      id: Identifier                          = id
    ): Module = {
      val res =
        Module(imports, exports, bindings, location, passData, diagnostics)
      res.id = id
      res
    }

    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Module =
      copy(
        imports = imports.map(
          _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
        ),
        bindings = bindings.map(
          _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
        ),
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = randomId
      )

    override def setLocation(location: Option[IdentifiedLocation]): Module =
      copy(location = location)

    override def mapExpressions(fn: Expression => Expression): Module = {
      copy(
        imports  = imports.map(_.mapExpressions(fn)),
        exports  = exports.map(_.mapExpressions(fn)),
        bindings = bindings.map(_.mapExpressions(fn))
      )
    }

    override def children: List[IR] = imports ++ exports ++ bindings

    override def toString: String =
      s"""
      |IR.Module(
      |imports = $imports,
      |exports = $exports,
      |bindings = $bindings,
      |location = $location,
      |passData = ${this.showPassData},
      |diagnostics = $diagnostics,
      |id = $id
      |)
      |""".toSingleLine

    override def showCode(indent: Int): String = {
      val importsString = imports.map(_.showCode(indent)).mkString("\n")

      val exportsString = exports.map(_.showCode(indent)).mkString("\n")

      val defsString = bindings.map(_.showCode(indent)).mkString("\n\n")

      List(importsString, exportsString, defsString).mkString("\n\n")
    }
  }

  object Module {

    /** A representation of constructs that can only occur in the top-level
      * module scope
      */
    sealed trait Scope extends IR {
      override def mapExpressions(fn: Expression => Expression):      Scope
      override def setLocation(location: Option[IdentifiedLocation]): Scope
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Scope
    }
    object Scope {

      /** An export statement.
        *
        * @param name the full path representing the export
        * @param rename the name this export is visible as
        * @param isAll is this an unqualified export
        * @param onlyNames exported names selected from the exported module
        * @param hiddenNames exported names hidden from the exported module
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Export(
        name: IR.Name.Qualified,
        rename: Option[IR.Name.Literal],
        isAll: Boolean,
        onlyNames: Option[List[IR.Name.Literal]],
        hiddenNames: Option[List[IR.Name.Literal]],
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends IR
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param name the full path representing the export
          * @param rename the name this export is visible as
          * @param isAll is this an unqualified export
          * @param onlyNames exported names selected from the exported module
          * @param hiddenNames exported names hidden from the exported module
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          name: IR.Name.Qualified                    = name,
          rename: Option[IR.Name.Literal]            = rename,
          isAll: Boolean                             = isAll,
          onlyNames: Option[List[IR.Name.Literal]]   = onlyNames,
          hiddenNames: Option[List[IR.Name.Literal]] = hiddenNames,
          location: Option[IdentifiedLocation]       = location,
          passData: MetadataStorage                  = passData,
          diagnostics: DiagnosticStorage             = diagnostics,
          id: Identifier                             = id
        ): Export = {
          val res = Export(
            name,
            rename,
            isAll,
            onlyNames,
            hiddenNames,
            location,
            passData,
            diagnostics
          )
          res.id = id
          res
        }

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Export =
          copy(
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Export =
          copy(location = location)

        override def mapExpressions(
          fn: Expression => Expression
        ): Export = this

        override def toString: String =
          s"""
               |IR.Module.Scope.Export(
               |name = $name,
               |rename = $rename,
               |isAll = $isAll,
               |onlyNames = $onlyNames,
               |hidingNames = $hiddenNames,
               |location = $location,
               |passData = ${this.showPassData},
               |diagnostics = $diagnostics,
               |id = $id
               |)
               |""".toSingleLine

        override def children: List[IR] =
          name :: List(
            rename.toList,
            onlyNames.getOrElse(List()),
            hiddenNames.getOrElse(List())
          ).flatten

        override def showCode(indent: Int): String = {
          val renameCode = rename.map(n => s" as ${n.name}").getOrElse("")
          if (isAll) {
            val onlyPart = onlyNames
              .map(names => " " + names.map(_.name).mkString(", "))
              .getOrElse("")
            val hidingPart = hiddenNames
              .map(names => s" hiding ${names.map(_.name).mkString(", ")}")
              .getOrElse("")
            val all = if (onlyNames.isDefined) "" else " all"
            s"from ${name.name}$renameCode export$onlyPart$all$hidingPart"
          } else {
            s"export ${name.name}$renameCode"
          }
        }

        /** Gets the name of the module visible in the importing scope,
          * either the original name or the rename.
          *
          * @return the name of this export visible in code
          */
        def getSimpleName: IR.Name = rename.getOrElse(name.parts.last)

        /** Checks whether the export statement allows use of the given
          * exported name.
          *
          * Note that it does not verify if the name is actually exported
          * by the module, only checks if it is syntactically allowed.
          *
          * @param name the name to check
          * @return whether the name could be accessed or not
          */
        def allowsAccess(name: String): Boolean = {
          if (!isAll) return false;
          if (onlyNames.isDefined) {
            onlyNames.get.exists(_.name.toLowerCase == name.toLowerCase)
          } else if (hiddenNames.isDefined) {
            !hiddenNames.get.exists(_.name.toLowerCase == name.toLowerCase)
          } else {
            true
          }
        }
      }

      /** Module-level import statements. */
      sealed trait Import extends Scope {
        override def mapExpressions(fn: Expression => Expression):      Import
        override def setLocation(location: Option[IdentifiedLocation]): Import
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Import
      }

      object Import {

        /** An import statement.
          *
          * @param name the full path representing the import
          * @param rename the name this import is visible as
          * @param isAll is this importing exported names
          * @param onlyNames exported names selected from the imported module
          * @param hiddenNames exported names hidden from the imported module
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Module(
          name: IR.Name.Qualified,
          rename: Option[IR.Name.Literal],
          isAll: Boolean,
          onlyNames: Option[List[IR.Name.Literal]],
          hiddenNames: Option[List[IR.Name.Literal]],
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Import
            with IRKind.Primitive {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param name the full `.`-separated path representing the import
            * @param rename the name this import is visible as
            * @param isAll is this importing exported names
            * @param onlyNames exported names selected from the imported module
            * @param hiddenNames exported names hidden from the imported module
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            name: IR.Name.Qualified                    = name,
            rename: Option[IR.Name.Literal]            = rename,
            isAll: Boolean                             = isAll,
            onlyNames: Option[List[IR.Name.Literal]]   = onlyNames,
            hiddenNames: Option[List[IR.Name.Literal]] = hiddenNames,
            location: Option[IdentifiedLocation]       = location,
            passData: MetadataStorage                  = passData,
            diagnostics: DiagnosticStorage             = diagnostics,
            id: Identifier                             = id
          ): Module = {
            val res = Module(
              name,
              rename,
              isAll,
              onlyNames,
              hiddenNames,
              location,
              passData,
              diagnostics
            )
            res.id = id
            res
          }

          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Module =
            copy(
              location = if (keepLocations) location else None,
              passData =
                if (keepMetadata) passData.duplicate else MetadataStorage(),
              diagnostics =
                if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
              id = randomId
            )

          override def setLocation(
            location: Option[IdentifiedLocation]
          ): Module =
            copy(location = location)

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

          override def children: List[IR] =
            name :: List(
              rename.toList,
              onlyNames.getOrElse(List()),
              hiddenNames.getOrElse(List())
            ).flatten

          override def showCode(indent: Int): String = {
            val renameCode = rename.map(n => s" as ${n.name}").getOrElse("")
            if (isAll) {
              val onlyPart = onlyNames
                .map(names => " " + names.map(_.name).mkString(", "))
                .getOrElse("")
              val hidingPart = hiddenNames
                .map(names => s" hiding ${names.map(_.name).mkString(", ")}")
                .getOrElse("")
              val all = if (onlyNames.isDefined) "" else " all"
              s"from ${name.name}$renameCode import$onlyPart$all$hidingPart"
            } else {
              s"import ${name.name}$renameCode"
            }
          }

          /** Gets the name of the module visible in this scope, either the
            * original name or the rename.
            *
            * @return the name of this import visible in code
            */
          def getSimpleName: IR.Name = rename.getOrElse(name.parts.last)

          /** Checks whether the import statement allows use of the given
            * exported name.
            *
            * Note that it does not verify if the name is actually exported
            * by the module, only checks if it is syntactically allowed.
            *
            * @param name the name to check
            * @return whether the name could be accessed or not
            */
          def allowsAccess(name: String): Boolean = {
            if (!isAll) return false;
            if (onlyNames.isDefined) {
              onlyNames.get.exists(_.name == name)
            } else if (hiddenNames.isDefined) {
              !hiddenNames.get.exists(_.name == name)
            } else {
              true
            }
          }
        }

        object Polyglot {

          /** Represents language-specific polyglot import data. */
          sealed trait Entity {
            val langName: String

            /** Returns the name this object is visible as from Enso code.
              *
              * @return the visible name of this object
              */
            def getVisibleName: String

            def showCode(indent: Int = 0): String
          }

          /** Represents an import of a Java class.
            *
            * @param packageName the name of the package containing the imported
            *                    class
            * @param className the class name
            */
          case class Java(packageName: String, className: String)
              extends Entity {
            val langName = "java"

            override def getVisibleName: String = className

            /** Returns the fully qualified Java name of this object.
              *
              * @return the Java-side name of the imported entity
              */
            def getJavaName: String = s"$packageName.$className"

            override def showCode(indent: Int): String =
              s"$packageName.$className"
          }
        }

        /** An import of a polyglot class.
          *
          * @param entity language-specific information on the imported entity
          * @param rename the name this object should be visible under in the
          *               importing scope
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Polyglot(
          entity: Polyglot.Entity,
          rename: Option[String],
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Import
            with IRKind.Primitive {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param entity language-specific information on the imported entity
            * @param rename the name this object should be visible under in the
            *               importing scope
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            entity: Polyglot.Entity              = entity,
            rename: Option[String]               = rename,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Polyglot = {
            val res =
              Polyglot(entity, rename, location, passData, diagnostics)
            res.id = id
            res
          }

          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Polyglot =
            copy(
              location = if (keepLocations) location else None,
              passData =
                if (keepMetadata) passData.duplicate else MetadataStorage(),
              diagnostics =
                if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
              id = randomId
            )

          override def setLocation(
            location: Option[IdentifiedLocation]
          ): Polyglot = copy(location = location)

          override def mapExpressions(fn: Expression => Expression): Polyglot =
            this

          /** Returns the name this object is visible as from Enso code.
            *
            * @return the visible name of this object
            */
          def getVisibleName: String = rename.getOrElse(entity.getVisibleName)

          override def toString: String =
            s"""
            |IR.Module.Scope.Import.Polyglot(
            |entity = $entity,
            |rename = $rename,
            |location = $location,
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List()

          override def showCode(indent: Int): String = {
            val renamePart = rename.map(name => s"as $name").getOrElse("")
            s"polyglot ${entity.langName} import ${entity.showCode(indent)} $renamePart"
          }
        }
      }

      /** A representation of top-level definitions. */
      sealed trait Definition extends Scope {
        override def mapExpressions(fn: Expression => Expression): Definition
        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Definition
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Definition
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

          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Atom =
            copy(
              name =
                name.duplicate(keepLocations, keepMetadata, keepDiagnostics),
              arguments = arguments.map(
                _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
              ),
              location = if (keepLocations) location else None,
              passData =
                if (keepMetadata) passData.duplicate else MetadataStorage(),
              diagnostics =
                if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
              id = randomId
            )

          override def setLocation(location: Option[IdentifiedLocation]): Atom =
            copy(location = location)

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

          override def showCode(indent: Int): String = {
            val fields = arguments.map(_.showCode(indent)).mkString(" ")

            s"type ${name.showCode(indent)} $fields"
          }
        }

        /** The definition of a complex type definition that may contain
          * multiple atom and method definitions.
          *
          * @param name the name of the complex type
          * @param arguments the (type) arguments to the complex type
          * @param body the body of the complex type
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          */
        sealed case class Type(
          name: IR.Name,
          arguments: List[DefinitionArgument],
          body: List[IR],
          override val location: Option[IdentifiedLocation],
          override val passData: MetadataStorage      = MetadataStorage(),
          override val diagnostics: DiagnosticStorage = DiagnosticStorage()
        ) extends Definition
            with IRKind.Sugar {
          override protected var id: Identifier = randomId

          /** Creates a copy of `this`.
            *
            * @param name the name of the complex type
            * @param arguments the (type) arguments to the complex type
            * @param body the body of the complex type
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            * @param id the identifier for the new node
            * @return a copy of `this`, updated with the specified values
            */
          def copy(
            name: IR.Name                        = name,
            arguments: List[DefinitionArgument]  = arguments,
            body: List[IR]                       = body,
            location: Option[IdentifiedLocation] = location,
            passData: MetadataStorage            = passData,
            diagnostics: DiagnosticStorage       = diagnostics,
            id: Identifier                       = id
          ): Type = {
            val res = Type(
              name,
              arguments,
              body,
              location,
              passData,
              diagnostics
            )
            res.id = id
            res
          }

          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Type =
            copy(
              name =
                name.duplicate(keepLocations, keepMetadata, keepDiagnostics),
              arguments = arguments.map(
                _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
              ),
              body = body.map(
                _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
              ),
              location = if (keepLocations) location else None,
              passData =
                if (keepMetadata) passData.duplicate else MetadataStorage(),
              diagnostics =
                if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
              id = randomId
            )

          override def mapExpressions(fn: Expression => Expression): Type =
            copy(body = body.map(_.mapExpressions(fn)))

          override def setLocation(
            location: Option[IdentifiedLocation]
          ): Type = copy(location = location)

          override def toString: String =
            s"""
            |IR.Module.Scope.Definition.Type(
            |name = $name,
            |arguments = $arguments,
            |body = $body,
            |location = $location,
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = (name :: arguments) ::: body

          override def showCode(indent: Int): String = {
            val headerArgs = arguments.map(_.showCode(indent)).mkString(" ")
            val header     = s"type ${name.name} $headerArgs"
            val newIndent  = indent + indentLevel
            val bodyStr = body
              .map(mkIndent(newIndent) + _.showCode(newIndent))
              .mkString("\n\n")

            s"$header\n$bodyStr"
          }
        }

        /** A trait representing method definitions in Enso. */
        sealed trait Method extends Definition {
          val methodReference: IR.Name.MethodReference
          val body: Expression

          override def setLocation(location: Option[IdentifiedLocation]): Method
          override def mapExpressions(fn: Expression => Expression):      Method
          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Method

          def typeName: IR.Name   = methodReference.typePointer
          def methodName: IR.Name = methodReference.methodName
        }
        object Method {

          /** The definition of a method for a given constructor.
            *
            * @param methodReference a reference to the method being defined
            * @param body the body of the method
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            */
          sealed case class Explicit(
            override val methodReference: IR.Name.MethodReference,
            override val body: Expression,
            override val location: Option[IdentifiedLocation],
            override val passData: MetadataStorage      = MetadataStorage(),
            override val diagnostics: DiagnosticStorage = DiagnosticStorage()
          ) extends Method
              with IRKind.Primitive {
            override protected var id: Identifier = _

            /** Creates a copy of `this`.
              *
              * @param methodReference a reference to the method being defined
              * @param body the body of the method
              * @param location the source location that the node corresponds to
              * @param passData the pass metadata associated with this node
              * @param diagnostics compiler diagnostics for this node
              * @param id the identifier for the new node
              * @return a copy of `this`, updated with the specified values
              */
            def copy(
              methodReference: IR.Name.MethodReference = methodReference,
              body: Expression                         = body,
              location: Option[IdentifiedLocation]     = location,
              passData: MetadataStorage                = passData,
              diagnostics: DiagnosticStorage           = diagnostics,
              id: Identifier                           = id
            ): Explicit = {
              val res = Explicit(
                methodReference,
                body,
                location,
                passData,
                diagnostics
              )
              res.id = id
              res
            }

            override def duplicate(
              keepLocations: Boolean   = true,
              keepMetadata: Boolean    = true,
              keepDiagnostics: Boolean = true
            ): Explicit =
              copy(
                methodReference = methodReference
                  .duplicate(keepLocations, keepMetadata, keepDiagnostics),
                body =
                  body.duplicate(keepLocations, keepMetadata, keepDiagnostics),
                location = if (keepLocations) location else None,
                passData =
                  if (keepMetadata) passData.duplicate else MetadataStorage(),
                diagnostics =
                  if (keepDiagnostics) diagnostics.copy
                  else DiagnosticStorage(),
                id = randomId
              )

            override def setLocation(
              location: Option[IdentifiedLocation]
            ): Explicit =
              copy(location = location)

            override def mapExpressions(
              fn: Expression => Expression
            ): Explicit = {
              copy(
                methodReference = methodReference.mapExpressions(fn),
                body            = fn(body)
              )
            }

            override def toString: String =
              s"""
              |IR.Module.Scope.Definition.Method.Explicit(
              |methodReference = $methodReference,
              |body = $body,
              |location = $location,
              |passData = ${this.showPassData},
              |diagnostics = $diagnostics,
              |id = $id
              |)
              |""".toSingleLine

            override def children: List[IR] = List(methodReference, body)

            override def showCode(indent: Int): String = {
              val exprStr = if (body.isInstanceOf[IR.Expression.Block]) {
                s"\n${body.showCode(indent)}"
              } else {
                s"${body.showCode(indent)}"
              }

              s"${methodReference.showCode(indent)} = $exprStr"
            }
          }

          /** The definition of a method for a given constructor using sugared
            * syntax.
            *
            * @param methodReference a reference to the method being defined
            * @param arguments the arguments to the method
            * @param body the body of the method
            * @param location the source location that the node corresponds to
            * @param passData the pass metadata associated with this node
            * @param diagnostics compiler diagnostics for this node
            */
          sealed case class Binding(
            override val methodReference: IR.Name.MethodReference,
            arguments: List[IR.DefinitionArgument],
            override val body: Expression,
            override val location: Option[IdentifiedLocation],
            override val passData: MetadataStorage      = MetadataStorage(),
            override val diagnostics: DiagnosticStorage = DiagnosticStorage()
          ) extends Method
              with IRKind.Sugar {
            override protected var id: Identifier = randomId

            /** Creates a copy of `this`.
              *
              * @param methodReference a reference to the method being defined
              * @param arguments the arguments to the method
              * @param body the body of the method
              * @param location the source location that the node corresponds to
              * @param passData the pass metadata associated with this node
              * @param diagnostics compiler diagnostics for this node
              * @param id the identifier for the new node
              * @return a copy of `this`, updated with the specified values
              */
            def copy(
              methodReference: IR.Name.MethodReference = methodReference,
              arguments: List[IR.DefinitionArgument]   = arguments,
              body: Expression                         = body,
              location: Option[IdentifiedLocation]     = location,
              passData: MetadataStorage                = passData,
              diagnostics: DiagnosticStorage           = diagnostics,
              id: Identifier                           = id
            ): Binding = {
              val res = Binding(
                methodReference,
                arguments,
                body,
                location,
                passData,
                diagnostics
              )
              res.id = id
              res
            }

            override def duplicate(
              keepLocations: Boolean   = true,
              keepMetadata: Boolean    = true,
              keepDiagnostics: Boolean = true
            ): Binding =
              copy(
                methodReference = methodReference
                  .duplicate(keepLocations, keepMetadata, keepDiagnostics),
                arguments = arguments.map(
                  _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
                ),
                body =
                  body.duplicate(keepLocations, keepMetadata, keepDiagnostics),
                location = if (keepLocations) location else None,
                passData =
                  if (keepMetadata) passData.duplicate else MetadataStorage(),
                diagnostics =
                  if (keepDiagnostics) diagnostics.copy
                  else DiagnosticStorage(),
                id = randomId
              )

            override def setLocation(
              location: Option[IdentifiedLocation]
            ): Binding =
              copy(location = location)

            override def mapExpressions(
              fn: Expression => Expression
            ): Binding = {
              copy(
                methodReference = methodReference.mapExpressions(fn),
                arguments       = arguments.map(_.mapExpressions(fn)),
                body            = fn(body)
              )
            }

            override def toString: String =
              s"""
              |IR.Module.Scope.Definition.Method.Binding(
              |methodReference = $methodReference,
              |arguments = $arguments,
              |body = $body,
              |location = $location,
              |passData = ${this.showPassData},
              |diagnostics = $diagnostics,
              |id = $id
              |)
              |""".toSingleLine

            override def children: List[IR] =
              (methodReference :: arguments) :+ body

            override def showCode(indent: Int): String = {
              val exprStr = if (body.isInstanceOf[IR.Expression.Block]) {
                s"\n${body.showCode(indent)}"
              } else {
                s"${body.showCode(indent)}"
              }

              val argsStr = arguments.map(_.showCode(indent)).mkString(" ")

              s"${methodReference.showCode(indent)} $argsStr = $exprStr"
            }
          }
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

    override def mapExpressions(fn: Expression => Expression):      Expression
    override def setLocation(location: Option[IdentifiedLocation]): Expression
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Expression
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Block =
        copy(
          expressions = expressions.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          returnValue =
            returnValue.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Block =
        copy(location = location)

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

      override def showCode(indent: Int): String = {
        val newIndent = indent + indentLevel
        val expressionsStr = expressions
          .map(mkIndent(newIndent) + _.showCode(newIndent))
          .mkString("\n")
        val returnStr = mkIndent(newIndent) + returnValue.showCode(newIndent)

        s"$expressionsStr\n$returnStr"
      }
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Binding =
        copy(
          name = name.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          expression =
            expression.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Binding =
        copy(location = location)

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

      override def showCode(indent: Int): String =
        s"${name.showCode(indent)} = ${expression.showCode(indent)}"
    }
  }

  // === Literals =============================================================

  /** Enso literals. */
  sealed trait Literal extends Expression with IRKind.Primitive {
    override def mapExpressions(fn: Expression => Expression):      Literal
    override def setLocation(location: Option[IdentifiedLocation]): Literal
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Literal
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Number =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Number =
        copy(location = location)

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

      override def showCode(indent: Int): String = value

      /** Checks whether the literal represents a fractional value.
        *
        * @return `true` if the value is fractional, `false` otherwise.
        */
      def isFractional: Boolean = value.contains(".")
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Text =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Text =
        copy(location = location)

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

      override def showCode(indent: Int): String = text
    }
  }

  // === Names ================================================================

  /** Enso names. */
  sealed trait Name extends Expression with IRKind.Primitive {
    val name: String

    override def mapExpressions(fn: Expression => Expression):      Name
    override def setLocation(location: Option[IdentifiedLocation]): Name
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Name

    /** Checks whether a name is in referent form.
      *
      * Please see the syntax specification for more details on this form.
      *
      * @return `true` if `this` is in referent form, otherwise `false`
      */
    def isReferent: Boolean

    /** Checks whether a name is in variable form.
      *
      * Please see the syntax specification for more details on this form.
      *
      * @return `true` if `this` is in referent form, otherwise `false`
      */
    def isVariable: Boolean = !isReferent
  }
  object Name {

    /** A representation of a method reference of the form `Type_Path.method`.
      *
      * @param typePointer the type name
      * @param methodName the method on `typeName`
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class MethodReference(
      typePointer: IR.Name,
      methodName: IR.Name,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name
        with IRKind.Sugar {
      override val name: String             = showCode()
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param typePointer the type name
        * @param methodName the method on `typeName`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        typePointer: IR.Name                 = typePointer,
        methodName: IR.Name                  = methodName,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): MethodReference = {
        val res =
          MethodReference(
            typePointer,
            methodName,
            location,
            passData,
            diagnostics
          )
        res.id = id
        res
      }

      override def isReferent: Boolean = true

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): MethodReference =
        copy(
          typePointer =
            typePointer.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          methodName =
            methodName.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def mapExpressions(
        fn: Expression => Expression
      ): MethodReference =
        copy(
          typePointer = typePointer.mapExpressions(fn),
          methodName  = methodName.mapExpressions(fn)
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): MethodReference = {
        copy(location = location)
      }

      override def toString: String =
        s"""
        |IR.Name.MethodReference(
        |typePointer = $typePointer,
        |methodName = $methodName,
        |location = $location,
        |passData = $passData,
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(typePointer, methodName)

      override def showCode(indent: Int): String = {
        val tPointer = typePointer.showCode(indent)
        s"$tPointer.${methodName.showCode(indent)}"
      }

      /** Checks whether `this` and `that` reference the same method.
        *
        * @param that the other method reference to check against
        * @return `true`, if `this` and `that` represent the same method,
        *         otherwise `false`
        */
      def isSameReferenceAs(that: MethodReference): Boolean = {
        typePointer.name == that.typePointer.name && this.methodName.name == that.methodName.name
      }
    }
    object MethodReference {

      /** Generates a location for the reference from the segments.
        *
        * @param segments the reference segments
        * @return a location for the method reference
        */
      def genLocation(segments: List[IR.Name]): Option[IdentifiedLocation] = {
        segments.foldLeft(None: Option[IdentifiedLocation])(
          (identLoc, segment) => {
            identLoc.flatMap(loc => {
              Some(
                IdentifiedLocation(
                  Location(
                    loc.location.start,
                    segment.location
                      .flatMap(l => Some(l.location.end))
                      .getOrElse(loc.location.end)
                  )
                )
              )
            })
          }
        )
      }
    }

    /** A representation of a qualified (multi-part) name.
      *
      * @param parts the segments of the name
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @return a copy of `this`, updated with the specified values
      */
    sealed case class Qualified(
      parts: List[IR.Name],
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name
        with IRKind.Primitive {

      override val name: String = parts.map(_.name).mkString(".")

      override def mapExpressions(fn: Expression => Expression): Name = this

      override def setLocation(location: Option[IdentifiedLocation]): Name =
        copy(location = location)

      override def isReferent: Boolean = true

      /** Creates a copy of `this`.
        *
        * @param parts the segments of the name
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        parts: List[IR.Name]                 = parts,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Qualified = {
        val res =
          Qualified(
            parts,
            location,
            passData,
            diagnostics
          )
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Qualified =
        copy(
          parts = parts.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def children: List[IR] = parts

      override protected var id: Identifier = randomId

      override def showCode(indent: Int): String = name
    }

    /** Represents occurrences of blank (`_`) expressions.
      *
      * @param location the source location that the node corresponds to.
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

      /** Creates a copy of `this`.
        *
        * @param location the source location that the node corresponds to.
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the node
        * @return a copy of `this`, updated with the specified values
        */
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

      override def isReferent: Boolean = false

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Blank =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def mapExpressions(fn: Expression => Expression): Blank =
        this

      override def setLocation(location: Option[IdentifiedLocation]): Blank =
        copy(location = location)

      override def toString: String =
        s"""
           |IR.Name.Blank(
           |location = $location,
           |passData = ${this.showPassData},
           |diagnostics = $diagnostics,
           |id = $id
           |)
           |""".stripMargin

      override def children: List[IR] = List()

      override def showCode(indent: Int): String = "_"
    }

    /** The representation of a literal name.
      *
      * @param name the literal text of the name
      * @param isReferent is this a referent name
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Literal(
      override val name: String,
      override val isReferent: Boolean,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the literal text of the name
        * @param isReferent is this a referent name
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: String                         = name,
        isReferent: Boolean                  = isReferent,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Literal = {
        val res = Literal(name, isReferent, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Literal =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Literal =
        copy(location = location)

      override def mapExpressions(fn: Expression => Expression): Literal = this

      override def toString: String =
        s"""
        |IR.Name.Literal(
        |name = $name,
        |isReferent = $isReferent,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()

      override def showCode(indent: Int): String = name
    }

    /** The representation of an annotation name.
      *
      * @param name the annotation text of the name
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Annotation(
      override val name: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Name {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the annotation text of the name
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
      ): Annotation = {
        val res = Annotation(name, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Annotation =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Annotation =
        copy(location = location)

      override def mapExpressions(fn: Expression => Expression): Annotation =
        this

      override def toString: String =
        s"""
           |IR.Name.Annotation(
           |name = $name,
           |location = $location,
           |passData = ${this.showPassData},
           |diagnostics = $diagnostics,
           |id = $id
           |)
           |""".toSingleLine

      override def children: List[IR] = List()

      override def isReferent: Boolean = false

      override def showCode(indent: Int): String = name
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

      override def isReferent: Boolean = false

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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): This =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): This =
        copy(location = location)

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

      override def showCode(indent: Int): String = "this"
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

      override def isReferent: Boolean = false

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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Here =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Here =
        copy(location = location)

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

      override def showCode(indent: Int): String = "here"
    }
  }

  // === Typing ===============================================================

  /** Constructs that operate on types. */
  sealed trait Type extends Expression {
    override def mapExpressions(fn: Expression => Expression):      Type
    override def setLocation(location: Option[IdentifiedLocation]): Type
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Type
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
        with Module.Scope.Definition
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Ascription =
        copy(
          typed = typed.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          signature =
            signature.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Ascription = copy(location = location)

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

      override def showCode(indent: Int): String =
        s"${typed.showCode(indent)} : ${signature.showCode(indent)}"
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Context =
        copy(
          typed = typed.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          context =
            context.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Context =
        copy(location = location)

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

      override def showCode(indent: Int): String =
        s"${typed.showCode(indent)} in ${context.showCode(indent)}"
    }
    object Context extends Info {
      override val name: String = "in"
    }

    /** Represents the ascription of an error context to an expression.
      *
      * @param typed the expression being ascribed an error context
      * @param error the error being ascribed
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Error(
      typed: Expression,
      error: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Type
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param typed the expression being ascribed an error context
        * @param error the error being ascribed
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        typed: Expression                    = typed,
        error: Expression                    = error,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Error = {
        val res = Error(typed, error, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Error =
        copy(
          typed    = typed.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          error    = error.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Error =
        copy(location = location)

      override def mapExpressions(fn: Expression => Expression): Error =
        copy(typed = fn(typed), error = fn(error))

      override def toString: String =
        s"""IR.Type.Error(
        |typed = $typed,
        |error = $error,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List(typed, error)

      override def showCode(indent: Int): String =
        s"(${typed.showCode(indent)} ! ${error.showCode(indent)})"
    }
    object Error extends Info {
      override val name: String = "!"
    }

    /** IR nodes for dealing with typesets. */
    sealed trait Set extends Type {
      override def mapExpressions(fn: Expression => Expression):      Set
      override def setLocation(location: Option[IdentifiedLocation]): Set
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Set
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Member =
          copy(
            label =
              label.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            memberType = memberType
              .duplicate(keepLocations, keepMetadata, keepDiagnostics),
            value =
              value.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(location: Option[IdentifiedLocation]): Member =
          copy(location = location)

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

        override def showCode(indent: Int): String = {
          val typeString  = s" : ${memberType.showCode(indent)}"
          val valueString = s" = ${value.showCode(indent)}"
          s"(${label.showCode(indent)}$typeString$valueString)"
        }
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Subsumption =
          copy(
            left = left.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            right =
              right.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Subsumption = copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} <: ${right.showCode(indent)})"
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Equality =
          copy(
            left = left.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            right =
              right.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Equality = copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} ~ ${right.showCode(indent)}"
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Concat =
          copy(
            left = left.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            right =
              right.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(location: Option[IdentifiedLocation]): Concat =
          copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)}; ${right.showCode(indent)})"
      }
      object Concat extends Info {
        override val name: String = ";"
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Union =
          copy(
            left = left.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            right =
              right.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(location: Option[IdentifiedLocation]): Union =
          copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} | ${right.showCode(indent)})"
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Intersection =
          copy(
            left = left.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            right =
              right.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Intersection = copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} & ${right.showCode(indent)})"
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Subtraction =
          copy(
            left = left.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            right =
              right.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Subtraction = copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(${left.showCode(indent)} \\ ${right.showCode(indent)})"
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

    override def mapExpressions(fn: Expression => Expression):      Function
    override def setLocation(location: Option[IdentifiedLocation]): Function
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Function
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Lambda =
        copy(
          arguments = arguments.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          body     = body.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Lambda =
        copy(location = location)

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

      override def showCode(indent: Int): String = {
        val args = arguments.map(_.showCode(indent)).mkString(" ")
        val bodyStr = if (body.isInstanceOf[IR.Expression.Block]) {
          s"\n${body.showCode(indent)}"
        } else {
          s"${body.showCode(indent)}"
        }

        s"$args -> $bodyStr"
      }
    }

    /** A representation of the syntactic sugar for defining functions.
      *
      * @param name the name of the function
      * @param arguments the arguments to the function
      * @param body the body of the function
      * @param location the source location that the node corresponds to
      * @param canBeTCO whether or not the function can be tail-call optimised
      * @param passData the pass metadata associated with this node
      * @param diagnostics the compiler diagnostics for this node
      */
    sealed case class Binding(
      name: IR.Name,
      override val arguments: List[DefinitionArgument],
      override val body: Expression,
      override val location: Option[IdentifiedLocation],
      override val canBeTCO: Boolean              = true,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Function
        with IRKind.Sugar {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name of the function
        * @param arguments the arguments to the function
        * @param body the body of the function
        * @param location the source location that the node corresponds to
        * @param canBeTCO whether or not the function can be tail-call optimised
        * @param passData the pass metadata associated with this node
        * @param diagnostics the compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        name: IR.Name                        = name,
        arguments: List[DefinitionArgument]  = arguments,
        body: Expression                     = body,
        location: Option[IdentifiedLocation] = location,
        canBeTCO: Boolean                    = canBeTCO,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Binding = {
        val res =
          Binding(
            name,
            arguments,
            body,
            location,
            canBeTCO,
            passData,
            diagnostics
          )
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Binding =
        copy(
          name = name.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          arguments = arguments.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          body     = body.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Binding =
        copy(location = location)

      override def mapExpressions(fn: Expression => Expression): Binding =
        copy(
          name      = name.mapExpressions(fn),
          arguments = arguments.map(_.mapExpressions(fn)),
          body      = fn(body)
        )

      override def toString: String =
        s"""
        |IR.Function.Sugar(
        |name = $name,
        |arguments = $arguments,
        |body = $body,
        |location = $location,
        |canBeTCO = $canBeTCO,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = (name :: arguments) :+ body

      override def showCode(indent: Int): String = {
        val argsStr = arguments.map(_.showCode(indent)).mkString(" ")
        val bodyStr = if (body.isInstanceOf[IR.Expression.Block]) {
          s"\n${body.showCode(indent)}"
        } else {
          s"${body.showCode(indent)}"
        }

        s"${name.name} $argsStr = $bodyStr"
      }
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

    override def setLocation(
      location: Option[IdentifiedLocation]
    ): DefinitionArgument

    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Specified =
        copy(
          name = name.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          defaultValue = defaultValue.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Specified = copy(location = location)

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

      override def showCode(indent: Int): String =
        if (defaultValue.isDefined) {
          s"(${name.showCode(indent)} = ${defaultValue.get.showCode(indent)})"
        } else {
          s"${name.showCode(indent)}"
        }
    }
  }

  // === Applications =========================================================

  /** All function applications in Enso. */
  sealed trait Application extends Expression {
    override def mapExpressions(fn: Expression => Expression):      Application
    override def setLocation(location: Option[IdentifiedLocation]): Application
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Application
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Prefix =
        copy(
          function =
            function.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          arguments = arguments.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Prefix =
        copy(location = location)

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

      override def showCode(indent: Int): String = {
        val argStr = arguments.map(_.showCode(indent)).mkString(" ")

        s"((${function.showCode(indent)}) $argStr)"
      }
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Force =
        copy(
          target =
            target.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Force =
        copy(location = location)

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

      override def showCode(indent: Int): String =
        s"(FORCE ${target.showCode(indent)})"
    }

    /** Literal applications in Enso. */
    sealed trait Literal extends Application {
      override def mapExpressions(fn: Expression => Expression):      Literal
      override def setLocation(location: Option[IdentifiedLocation]): Literal
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Literal
    }

    object Literal {

      /** A representation of a typeset literal.
        *
        * These are necessary as they delimit pattern contexts.
        *
        * @param expression the expression of the typeset body
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        */
      sealed case class Typeset(
        expression: Option[Expression],
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Literal
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        override def mapExpressions(fn: Expression => Expression): Typeset =
          copy(expression = expression.map(fn))

        /** Creates a copy of `this`.
          *
          * @param expression the expression of the typeset body
          * @param location the source location that the node corresponds to
          * @param passData the pass metadata associated with this node
          * @param diagnostics compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updataed with the specified values
          */
        def copy(
          expression: Option[Expression]       = expression,
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): Typeset = {
          val res = Typeset(expression, location, passData, diagnostics)
          res.id = id
          res
        }

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Typeset =
          copy(
            expression = expression.map(
              _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Typeset = copy(location = location)

        override def toString: String =
          s"""IR.Application.Literal.Typeset(
          |expression = $expression,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |)
          |""".toSingleLine

        override def children: List[IR] =
          expression.map(List(_)).getOrElse(List())

        override def showCode(indent: Int): String = {
          val exprString = if (expression.isDefined) {
            expression.get.showCode(indent)
          } else ""

          s"{ $exprString }"
        }
      }

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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Sequence =
          copy(
            items = items.map(
              _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
            ),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Sequence = copy(location = location)

        override def toString: String =
          s"""
          |IR.Application.Literal.Vector(
          |items = $items,
          |location = $location,
          |passData = ${this.showPassData},
          |diagnostics = $diagnostics,
          |id = $id
          |)
          |""".toSingleLine

        override def children: List[IR] = items

        override def showCode(indent: Int): String = {
          val itemsStr = items.map(_.showCode(indent)).mkString(", ")
          s"[$itemsStr]"
        }
      }
    }

    /** Operator applications in Enso. */
    sealed trait Operator extends Application {
      override def mapExpressions(fn: Expression => Expression):      Operator
      override def setLocation(location: Option[IdentifiedLocation]): Operator
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Operator
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Binary =
          copy(
            left = left.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            operator =
              operator.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            right =
              right.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(location: Option[IdentifiedLocation]): Binary =
          copy(location = location)

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

        override def showCode(indent: Int): String = {
          val opStr = operator.showCode(indent)

          s"(${left.showCode(indent)} $opStr ${right.showCode(indent)})"
        }
      }

      /** Operator sections. */
      sealed trait Section extends Operator {
        override def mapExpressions(fn: Expression => Expression):      Section
        override def setLocation(location: Option[IdentifiedLocation]): Section
        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Section
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

          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Left =
            copy(
              arg = arg.duplicate(keepLocations, keepMetadata, keepDiagnostics),
              operator = operator
                .duplicate(keepLocations, keepMetadata, keepDiagnostics),
              location = if (keepLocations) location else None,
              passData =
                if (keepMetadata) passData.duplicate else MetadataStorage(),
              diagnostics =
                if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
              id = randomId
            )

          override def setLocation(location: Option[IdentifiedLocation]): Left =
            copy(location = location)

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
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List(arg, operator)

          override def showCode(indent: Int): String =
            s"(${arg.showCode(indent)} ${operator.showCode(indent)})"
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

          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Sides =
            copy(
              operator = operator
                .duplicate(keepLocations, keepMetadata, keepDiagnostics),
              location = if (keepLocations) location else None,
              passData =
                if (keepMetadata) passData.duplicate else MetadataStorage(),
              diagnostics =
                if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
              id = randomId
            )

          override def setLocation(
            location: Option[IdentifiedLocation]
          ): Sides = copy(location = location)

          override def mapExpressions(fn: Expression => Expression): Section =
            copy(operator = operator.mapExpressions(fn))

          override def toString: String =
            s"""
            |IR.Application.Operator.Section.Centre(
            |operator =  $operator,
            |location = $location,
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List(operator)

          override def showCode(indent: Int): String =
            s"(${operator.showCode(indent)})"
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

          override def duplicate(
            keepLocations: Boolean   = true,
            keepMetadata: Boolean    = true,
            keepDiagnostics: Boolean = true
          ): Right =
            copy(
              operator = operator
                .duplicate(keepLocations, keepMetadata, keepDiagnostics),
              arg      = arg.duplicate(keepLocations, keepMetadata, keepDiagnostics),
              location = if (keepLocations) location else None,
              passData =
                if (keepMetadata) passData.duplicate else MetadataStorage(),
              diagnostics =
                if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
              id = randomId
            )

          override def setLocation(
            location: Option[IdentifiedLocation]
          ): Right = copy(location = location)

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
            |passData = ${this.showPassData},
            |diagnostics = $diagnostics,
            |id = $id
            |)
            |""".toSingleLine

          override def children: List[IR] = List(operator, arg)

          override def showCode(indent: Int): String =
            s"(${operator.showCode(indent)} ${arg.showCode(indent)})"
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

    override def mapExpressions(fn: Expression => Expression):      CallArgument
    override def setLocation(location: Option[IdentifiedLocation]): CallArgument
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): CallArgument
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Specified =
        copy(
          name =
            name.map(_.duplicate(keepLocations, keepMetadata, keepDiagnostics)),
          value    = value.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Specified = copy(location = location)

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

      override def showCode(indent: Int): String = {
        if (name.isDefined) {
          s"(${name.get.showCode(indent)} = ${value.showCode(indent)})"
        } else {
          s"${value.showCode(indent)}"
        }
      }
    }
  }

  // === Case Expression ======================================================

  /** The Enso case expression. */
  sealed trait Case extends Expression {
    override def mapExpressions(fn: Expression => Expression):      Case
    override def setLocation(location: Option[IdentifiedLocation]): Case
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Case
  }
  object Case {

    /** The main body of the Enso case expression.
      *
      * @param scrutinee the expression whose value is being matched on
      * @param branches the branches of the case expression
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Expr(
      scrutinee: Expression,
      branches: Seq[Branch],
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
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        scrutinee: Expression                = scrutinee,
        branches: Seq[Branch]                = branches,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Expr = {
        val res =
          Expr(scrutinee, branches, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Expr =
        copy(
          scrutinee =
            scrutinee.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          branches = branches.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Expr =
        copy(location = location)

      override def mapExpressions(fn: Expression => Expression): Expr = {
        copy(
          scrutinee = fn(scrutinee),
          branches.map(_.mapExpressions(fn))
        )
      }

      override def toString: String =
        s"""
        |IR.Case.Expr(
        |scrutinee = $scrutinee,
        |branches = $branches,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = scrutinee :: branches.toList

      override def showCode(indent: Int): String = {
        val newIndent = indent + indentLevel
        val headerStr = s"case ${scrutinee.showCode(indent)} of"
        val branchesStr = branches
          .map(mkIndent(newIndent) + _.showCode(newIndent))
          .mkString("\n")

        s"$headerStr\n$branchesStr"
      }
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
      pattern: Pattern,
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
        pattern: Pattern                     = pattern,
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Branch =
        copy(
          pattern =
            pattern.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          expression =
            expression.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Branch =
        copy(location = location)

      override def mapExpressions(fn: Expression => Expression): Branch = {
        copy(pattern = pattern.mapExpressions(fn), expression = fn(expression))
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

      override def showCode(indent: Int): String = {
        val newIndent = indent + indentLevel
        val bodyStr = if (expression.isInstanceOf[IR.Expression.Block]) {
          s"\n${mkIndent(newIndent)}${expression.showCode(newIndent)}"
        } else {
          s"${expression.showCode(indent)}"
        }
        s"${pattern.showCode(indent)} -> $bodyStr"
      }
    }
  }

  // === Patterns =============================================================

  /** The different types of patterns that can occur in a match. */
  sealed trait Pattern extends IR {
    override def mapExpressions(fn: Expression => Expression):      Pattern
    override def setLocation(location: Option[IdentifiedLocation]): Pattern
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Pattern
  }
  object Pattern {

    /** A named pattern.
      *
      * Named patterns take the form of a single identifier (e.g. `a` or `_`).
      * As a result they can be used to represent a catch all pattern (e.g.
      * `_ -> ...` or `a -> ...`).
      *
      * @param name the name that constitutes the pattern
      * @param location the source location for this IR node
      * @param passData any pass metadata associated with the node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Name(
      name: IR.Name,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Pattern {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param name the name that constitutes the pattern
        * @param location the source location for this IR node
        * @param passData any pass metadata associated with the node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the provided values
        */
      def copy(
        name: IR.Name                        = name,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Name = {
        val res = Name(name, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Name =
        copy(
          name     = name.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def mapExpressions(fn: Expression => Expression): Name = {
        copy(name = name.mapExpressions(fn))
      }

      override def toString: String =
        s"""
        |IR.Case.Pattern.Name(
        |name = $name,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def setLocation(location: Option[IdentifiedLocation]): Name =
        copy(location = location)

      override def children: List[IR] = List(name)

      override def showCode(indent: Int): String = name.showCode(indent)
    }

    /** A pattern that destructures a constructor application.
      *
      * The first part of the pattern must be a refferent name. The fields of
      * the constructor may be any available kind of pattern.
      *
      * @param constructor the constructor being matched on
      * @param fields the asserted fields of the constructor
      * @param location the source location for this IR node
      * @param passData any pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Constructor(
      constructor: IR.Name,
      fields: List[IR.Pattern],
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Pattern {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param constructor the constructor being matched on
        * @param fields the asserted fields of the constructor
        * @param location the source location for this IR node
        * @param passData any pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the new identifier for this node
        * @return a copy of `this`, updated with the provided values
        */
      def copy(
        constructor: IR.Name                 = constructor,
        fields: List[IR.Pattern]             = fields,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Constructor = {
        val res =
          Constructor(constructor, fields, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Constructor =
        copy(
          constructor =
            constructor.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          fields = fields.map(
            _.duplicate(keepLocations, keepMetadata, keepDiagnostics)
          ),
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      /** Checks if the constructor pattern has been desugared.
        *
        * A constructor pattern has been desugared if all of its fields are
        * [[Pattern.Name]].
        *
        * @return `true` if the pattern has been desugared, `false` otherwise
        */
      def isDesugared: Boolean = {
        fields.forall {
          case _: Pattern.Name        => true
          case _: Pattern.Constructor => false
          case _: Pattern.Documentation =>
            throw new CompilerError(
              "Branch documentation should not be present " +
              "inside a constructor pattern."
            )
          case _: Error.Pattern => true
        }
      }

      /** Gets the patterns fields as [[Pattern.Name]] if they are.
        *
        * @return the fields from `this`
        */
      def fieldsAsNamed: List[Option[Pattern.Name]] = {
        fields.map {
          case f: Pattern.Name => Some(f)
          case _               => None
        }
      }

      /** Unsafely gets the pattern's fields as if they are [[Pattern.Name]].
        *
        * @return the fields from `this`
        */
      def unsafeFieldsAsNamed: List[Pattern.Name] = {
        fieldsAsNamed.map(_.get)
      }

      override def mapExpressions(fn: Expression => Expression): Constructor =
        copy(
          constructor = constructor.mapExpressions(fn),
          fields      = fields.map(_.mapExpressions(fn))
        )

      override def toString: String =
        s"""
        |IR.Case.Pattern.Constructor(
        |constructor = $constructor,
        |fields = $fields,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Constructor = copy(location = location)

      override def children: List[IR] = constructor :: fields

      override def showCode(indent: Int): String = {
        val fieldsStr =
          fields.map(f => s"(${f.showCode(indent)})").mkString(" ")

        s"${constructor.name} $fieldsStr"
      }
    }

    /** A dummy pattern used for storing documentation comments between branches
      * in a pattern match.
      *
      * To store a documentation comment next to a branch, a dummy branch is
      * created with its pattern being an instance of this Doc and expression
      * being empty.
      *
      * @param doc the documentation entity
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    final case class Documentation(
      doc: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Pattern {
      override protected var id: Identifier = randomId

      override def mapExpressions(fn: Expression => Expression): Documentation =
        this

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Documentation =
        copy(location = location)

      /** Creates a copy of `this`.
        *
        * @param doc the documentation entity
        * @param location the source location for this IR node
        * @param passData any pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the new identifier for this node
        * @return a copy of `this`, updated with the provided values
        */
      def copy(
        doc: String                          = doc,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Documentation = {
        val res = Documentation(doc, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean,
        keepMetadata: Boolean,
        keepDiagnostics: Boolean
      ): Documentation =
        copy(
          doc,
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def children: List[IR] = Nil

      override def toString: String =
        s"""
           |IR.Case.Pattern.Doc(
           |doc = $doc,
           |location = $location,
           |passData = ${this.showPassData},
           |diagnostics = $diagnostics,
           |id = $id
           |)
           |""".toSingleLine

      override def showCode(indent: Int): String = s"## $doc"
    }
  }

  // === Comments =============================================================

  /** Enso comment entities. */
  sealed trait Comment extends Expression with Module.Scope.Definition {
    override def mapExpressions(fn: Expression => Expression):      Comment
    override def setLocation(location: Option[IdentifiedLocation]): Comment
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Comment
  }
  object Comment {

    /** A documentation comment in the Enso source.
      *
      * @param doc the documentation entity
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      */
    sealed case class Documentation(
      doc: String,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Comment
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

      /** Creates a copy of `this`.
        *
        * @param doc the documentation of `commented`
        * @param location the source location that the node corresponds to
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        doc: String                          = doc,
        location: Option[IdentifiedLocation] = location,
        passData: MetadataStorage            = passData,
        diagnostics: DiagnosticStorage       = diagnostics,
        id: Identifier                       = id
      ): Documentation = {
        val res = Documentation(doc, location, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Documentation =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Documentation = copy(location = location)

      override def mapExpressions(
        fn: Expression => Expression
      ): Documentation = this

      override def toString: String =
        s"""
        |IR.Comment.Documentation(
        |doc = $doc,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()

      override def showCode(indent: Int): String =
        s"## $doc"
    }
  }

  // === Foreign ==============================================================

  /** Foreign code entities. */
  sealed trait Foreign extends Expression {
    override def mapExpressions(fn: Expression => Expression):      Foreign
    override def setLocation(location: Option[IdentifiedLocation]): Foreign
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Foreign
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Definition =
        copy(
          location = if (keepLocations) location else None,
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Definition = copy(location = location)

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

      override def showCode(indent: Int): String = "FOREIGN DEF"
    }
  }

  // === Diagnostics ==========================================================

  /** A representation of various kinds of diagnostic in the IR. */
  sealed trait Diagnostic {

    /** @return a human-readable description of this error condition.
      */
    def message: String

    /** The location at which the diagnostic occurs. */
    val location: Option[IdentifiedLocation]
  }
  object Diagnostic {

    /** Represents the various kinds of diagnostics in the IR. */
    sealed trait Kind
    object Kind {

      /** Diagnostics that should be reported during the static compilation
        * phase of execution.
        */
      sealed trait Static extends Kind

      /** Diagnostics that should remain at runtime for display during
        * interactive execution.
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

      sealed case class PatternBinding(override val name: Name) extends Unused {
        override def message: String = s"Unused pattern binding ${name.name}."

        override def toString: String = s"Unused.PatternBinding(${name.name})"

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

    /** Warnings for unreachable code. */
    sealed trait Unreachable extends Warning {
      val location: Option[IdentifiedLocation]
    }
    object Unreachable {

      /** A warning for unreachable branches in a case expression.
        *
        * @param location the location of the unreachable branches
        */
      sealed case class Branches(
        override val location: Option[IdentifiedLocation]
      ) extends Unreachable {
        val atLocation =
          if (location.isDefined) { s" at location ${location.get}" }
          else                    { ""                              }

        override def message: String = s"Unreachable case branches$atLocation."
      }
    }

    /** A warning about a `@Tail_Call` annotation placed in a non-tail
      * position.
      * @param location the location of the annotated application
      */
    case class WrongTco(override val location: Option[IdentifiedLocation])
        extends Warning {
      override def message: String =
        "A @Tail_Call annotation was placed in a non-tail-call position."
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
        * @param location the location at which the shadowing takes place
        */
      sealed case class FunctionParam(
        shadowedName: String,
        override val shadower: IR,
        override val location: Option[IdentifiedLocation]
      ) extends Shadowed {
        override def message: String =
          s"The argument $shadowedName is shadowed by $shadower."
      }

      /** A warning that a later-defined pattern variable shadows an
        * earlier-defined pattern variable.
        *
        * @param shadowedName the name being shadowed
        * @param shadower the expression shadowing `warnedExpr`
        * @param location the location at which the shadowing takes place
        */
      sealed case class PatternBinding(
        shadowedName: String,
        override val shadower: IR,
        override val location: Option[IdentifiedLocation]
      ) extends Shadowed {
        override def message: String =
          s"The pattern field $shadowedName is shadowed by $shadower."
      }
    }
  }

  // === Errors ===============================================================

  /** A trait for all errors in Enso's IR. */
  sealed trait Error extends Expression with Diagnostic {
    override def mapExpressions(fn: Expression => Expression):      Error
    override def setLocation(location: Option[IdentifiedLocation]): Error
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): Error
  }
  object Error {

    /** A representation of an error resulting from name resolution.
      *
      * @param originalName the original name that could not be resolved
      * @param reason the cause of this error
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @return a copy of `this`, updated with the specified values
      */
    sealed case class Resolution(
      originalName: IR.Name,
      reason: Resolution.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with IRKind.Primitive
        with IR.Name {
      override val name: String = originalName.name

      override def isReferent: Boolean = originalName.isReferent

      override def mapExpressions(fn: Expression => Expression): Resolution =
        this

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): Resolution =
        copy(originalName = originalName.setLocation(location))

      /** Creates a copy of `this`.
        *
        * @param originalName the original name that could not be resolved
        * @param reason the cause of this error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        originalName: IR.Name          = originalName,
        reason: Resolution.Reason      = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): Resolution = {
        val res = Resolution(originalName, reason, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Resolution =
        copy(
          originalName = originalName
            .duplicate(keepLocations, keepMetadata, keepDiagnostics),
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def children: List[IR] = List(originalName)

      override protected var id: Identifier = randomId

      override def showCode(indent: Int): String = originalName.showCode(indent)

      override def message: String = reason.explain(originalName)

      override val location: Option[IdentifiedLocation] = originalName.location
    }

    object Resolution {

      /** A representation of a symbol resolution error.
        */
      sealed trait Reason {
        def explain(originalName: IR.Name): String
      }

      case object UnresolvedSequenceMacro extends Reason {
        override def explain(originalName: Name): String =
          "No definition for the sequence macro could be found. Try" +
          " importing the default definition from the Base.Vector module."
      }

      /** An error coming from an unknown annotation name.
        */
      case object UnknownAnnotation extends Reason {
        override def explain(originalName: Name): String =
          s"The annotation ${originalName.name} is not defined."
      }

      /** An error coming from a tail call annotation placed in a syntactically
        * incorrect position.
        */
      case object UnexpectedTailCallAnnotation extends Reason {
        override def explain(originalName: Name): String =
          s"Unexpected @TailCall annotation. This annotation can only be " +
          s"used with function applications."
      }

      /** An error coming from an unexpected occurence of a polyglot symbol.
        *
        * @param context the description of a context in which the error
        *                happened.
        */
      case class UnexpectedPolyglot(context: String) extends Reason {
        override def explain(originalName: Name): String =
          s"The name ${originalName.name} resolved to a polyglot symbol, " +
          s"but polyglot symbols are not allowed in $context."
      }

      /** An error coming from an unexpected occurence of a static method.
        *
        * @param context the description of a context in which the error
        *                happened.
        */
      case class UnexpectedMethod(context: String) extends Reason {
        override def explain(originalName: Name): String =
          s"The name ${originalName.name} resolved to a method, " +
          s"but methods are not allowed in $context."
      }

      /** An error coming from name resolver.
        *
        * @param err the original error.
        */
      case class ResolverError(err: BindingsMap.ResolutionError)
          extends Reason {

        /** Provides a human-readable explanation of the error.
          * @param originalName the original unresolved name.
          * @return a human-readable message.
          */
        override def explain(originalName: IR.Name): String =
          err match {
            case BindingsMap.ResolutionAmbiguous(candidates) =>
              val firstLine =
                s"The name ${originalName.name} is ambiguous. Possible candidates are:"
              val lines = candidates.map {
                case BindingsMap.ResolvedConstructor(
                      definitionModule,
                      cons
                    ) =>
                  s"    Type ${cons.name} defined in module ${definitionModule.getName};"
                case BindingsMap.ResolvedModule(module) =>
                  s"    The module ${module.getName};"
                case BindingsMap.ResolvedPolyglotSymbol(_, symbol) =>
                  s"    The imported polyglot symbol ${symbol.name};"
                case BindingsMap.ResolvedMethod(module, symbol) =>
                  s"    The method ${symbol.name} defined in module ${module.getName}"
              }
              (firstLine :: lines).mkString("\n")
            case BindingsMap.ResolutionNotFound =>
              s"The name ${originalName.name} could not be found."
          }

      }
    }

    /** A representation of an error resulting from wrong pattern matches.
      *
      * @param originalPattern pattern that resulted in the error
      * @param reason the cause of this error
      * @param passData the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @return a copy of `this`, updated with the specified values
      */
    sealed case class Pattern(
      originalPattern: IR.Pattern,
      reason: Pattern.Reason,
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Error
        with Diagnostic.Kind.Interactive
        with IR.Pattern {
      override def mapExpressions(fn: Expression => Expression): Pattern =
        copy(originalPattern = originalPattern.mapExpressions(fn))

      override def setLocation(location: Option[IdentifiedLocation]): Pattern =
        copy(originalPattern = originalPattern.setLocation(location))

      /** Creates a copy of `this`.
        *
        * @param originalPattern the pattern that resulted in the error
        * @param reason the cause of this error
        * @param passData the pass metadata associated with this node
        * @param diagnostics compiler diagnostics for this node
        * @param id the identifier for the new node
        * @return a copy of `this`, updated with the specified values
        */
      def copy(
        originalPattern: IR.Pattern    = originalPattern,
        reason: Pattern.Reason         = reason,
        passData: MetadataStorage      = passData,
        diagnostics: DiagnosticStorage = diagnostics,
        id: Identifier                 = id
      ): Pattern = {
        val res = Pattern(originalPattern, reason, passData, diagnostics)
        res.id = id
        res
      }

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Pattern =
        copy(
          originalPattern = originalPattern
            .duplicate(keepLocations, keepMetadata, keepDiagnostics),
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def message: String = reason.explain

      override val location: Option[IdentifiedLocation] =
        originalPattern.location

      override def children: List[IR] = List(originalPattern)

      override protected var id: Identifier = randomId

      override def showCode(indent: Int): String =
        originalPattern.showCode(indent)
    }

    object Pattern {

      /** A representation of the reason the pattern is erroneous.
        */
      sealed trait Reason {

        /** Provides a human-readable explanation of the error.
          * @return
          */
        def explain: String
      }

      /** A reason for pattern failing due to wrong arity.
        *
        * @param consName the constructor name.
        * @param expected expected field count.
        * @param actual actual field count.
        */
      case class WrongArity(consName: String, expected: Int, actual: Int)
          extends Reason {
        override def explain: String =
          s"Wrong number of fields when matching on $consName." +
          s" Expected $expected fields, but provided $actual."
      }
    }

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
        with IR.Module.Scope.Definition
        with IR.Module.Scope.Import
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

      override def duplicate(
        @unused keepLocations: Boolean = true,
        keepMetadata: Boolean          = true,
        keepDiagnostics: Boolean       = true
      ): Syntax =
        copy(
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(location: Option[IdentifiedLocation]): Syntax =
        this

      override val location: Option[IdentifiedLocation] =
        ast.location.map(IdentifiedLocation(_, ast.id))

      override def mapExpressions(fn: Expression => Expression): Syntax = this

      override def toString: String =
        s"""
        |IR.Error.Syntax(
        |ast = $ast,
        |reason = $reason,
        |location = $location,
        |passData = ${this.showPassData},
        |diagnostics = $diagnostics,
        |id = $id
        |)
        |""".toSingleLine

      override def children: List[IR] = List()

      override def message: String = reason.explanation

      override def showCode(indent: Int): String = "Syntax_Error"
    }
    object Syntax {

      /** A common type for all syntax errors expected by the language.
        */
      sealed trait Reason {

        /** @return a human-readable description of the error.
          */
        def explanation: String
      }

      case class InvalidEscapeSequence(lit: String) extends Reason {
        override def explanation: String = s"Invalid escape sequence $lit."
      }

      case object InvalidBaseInDecimalLiteral extends Reason {
        override def explanation: String =
          "Cannot change base of the fractional part of a number literal."
      }

      case class UnsupportedSyntax(syntaxName: String) extends Reason {
        override def explanation: String =
          s"Syntax is not supported yet: $syntaxName."
      }

      case object InvalidPattern extends Reason {
        override def explanation: String =
          s"Cannot define a pattern outside a pattern context."
      }

      case object InvalidImport extends Reason {
        override def explanation: String =
          s"Imports must have a valid module path."
      }

      case object InvalidStandaloneSignature extends Reason {
        override def explanation: String =
          s"Invalid stand-alone signature expression."
      }

      case class MethodDefinedInline(methodName: String) extends Reason {
        override def explanation: String =
          s"Cannot define $methodName, methods are not supported in the " +
          s"inline flow."
      }

      case object UnexpectedDeclarationInType extends Reason {
        override def explanation: String =
          "Unexpected declaration in the body of a type."
      }

      case object InvalidTypeDefinition extends Reason {
        override def explanation: String =
          "Invalid definition of a type."
      }

      case object InterfaceDefinition extends Reason {
        override def explanation: String =
          "Interface definitions are not supported yet."
      }

      case class TypeDefinedInline(typeName: String) extends Reason {
        override def explanation: String =
          s"Cannot define $typeName, type definitions are not supported " +
          s"in the inline flow."
      }

      case object EmptyParentheses extends Reason {
        override def explanation: String = "Parentheses can't be empty."
      }

      case object UnexpectedExpression extends Reason {
        override def explanation: String = "Unexpected expression."
      }

      case object AmbiguousExpression extends Reason {
        override def explanation: String = "Ambiguous expression."
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

      case object InvalidOperatorName extends Reason {
        override def explanation: String = "Invalid operator name."
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

      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): InvalidIR =
        copy(
          ir = ir.duplicate(keepLocations, keepMetadata, keepDiagnostics),
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
          diagnostics =
            if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
          id = randomId
        )

      override def setLocation(
        location: Option[IdentifiedLocation]
      ): InvalidIR = this

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

      override def showCode(indent: Int): String = "Invalid_Ir"
    }

    /** Errors pertaining to the redefinition of language constructs that are
      * not allowed to be.
      */
    sealed trait Redefined extends Error {
      override def mapExpressions(fn: Expression => Expression):      Redefined
      override def setLocation(location: Option[IdentifiedLocation]): Redefined
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Redefined
    }
    object Redefined {

      /** An error representing the redefinition or incorrect positioning of
        * the `this` argument to methods.
        *
        * @param location the source location of the error
        * @param passData the pass metadata for this node
        * @param diagnostics compiler diagnostics associated with the node
        */
      sealed case class ThisArg(
        override val location: Option[IdentifiedLocation],
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Redefined
          with Diagnostic.Kind.Interactive
          with IRKind.Primitive {
        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param location the source location of the error
          * @param passData the pass metadata for this node
          * @param diagnostics compiler diagnostics associated with the node
          * @param id the node's identifier
          * @return a copy of `this`, with the specified values updated
          */
        def copy(
          location: Option[IdentifiedLocation] = location,
          passData: MetadataStorage            = passData,
          diagnostics: DiagnosticStorage       = diagnostics,
          id: Identifier                       = id
        ): ThisArg = {
          val res = ThisArg(location, passData, diagnostics)
          res.id = id
          res
        }

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): ThisArg =
          copy(
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): ThisArg = copy(location = location)

        override def mapExpressions(fn: Expression => Expression): ThisArg =
          this

        override def message: String =
          "Methods must have only one definition of the `this` argument, and " +
          "it must be the first."

        override def children: List[IR] = List()

        override def showCode(indent: Int): String = "(Redefined This_Arg)"
      }

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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Method =
          copy(
            atomName =
              atomName.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            methodName = methodName
              .duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(location: Option[IdentifiedLocation]): Method =
          copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(Redefined (Method $atomName.$methodName))"
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Atom =
          copy(
            atomName =
              atomName.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            location = if (keepLocations) location else None,
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(location: Option[IdentifiedLocation]): Atom =
          copy(location = location)

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

        override def showCode(indent: Int): String =
          s"(Redefined (Atom $atomName))"
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

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): Binding =
          copy(
            invalidBinding = invalidBinding
              .duplicate(keepLocations, keepMetadata, keepDiagnostics),
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): Binding = this

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

        override def showCode(indent: Int): String =
          s"(Redefined (Binding $invalidBinding))"
      }
    }

    /** A trait for errors about unexpected language constructs. */
    sealed trait Unexpected extends Error {

      /** The unexpected construct. */
      val ir: IR

      /** The name of the unexpected entity. */
      val entity: String

      override val location: Option[IdentifiedLocation] = ir.location

      override def message: String = s"Unexpected $entity."

      override def mapExpressions(fn: Expression => Expression):      Unexpected
      override def setLocation(location: Option[IdentifiedLocation]): Unexpected
      override def duplicate(
        keepLocations: Boolean   = true,
        keepMetadata: Boolean    = true,
        keepDiagnostics: Boolean = true
      ): Unexpected
    }
    object Unexpected {

      /** An error representing a type signature not associated with a
        * binding of some kind.
        *
        * @param ir the erroneous signature
        * @param passData any pass metadata associated with this node
        * @param diagnostics any compiler diagnostics for this node
        */
      sealed case class TypeSignature(
        override val ir: IR,
        override val passData: MetadataStorage      = MetadataStorage(),
        override val diagnostics: DiagnosticStorage = DiagnosticStorage()
      ) extends Unexpected
          with IRKind.Primitive
          with IR.Module.Scope.Definition {
        override val entity: String = "type signature"

        override protected var id: Identifier = randomId

        /** Creates a copy of `this`.
          *
          * @param ir the erroneous signature
          * @param passData any pass metadata associated with this node
          * @param diagnostics any compiler diagnostics for this node
          * @param id the identifier for the new node
          * @return a copy of `this`, updated with the specified values
          */
        def copy(
          ir: IR                         = ir,
          passData: MetadataStorage      = passData,
          diagnostics: DiagnosticStorage = diagnostics,
          id: Identifier                 = id
        ): TypeSignature = {
          val res = TypeSignature(ir, passData, diagnostics)
          res.id = id
          res
        }

        override def mapExpressions(
          fn: Expression => Expression
        ): TypeSignature = this

        override def setLocation(
          location: Option[IdentifiedLocation]
        ): TypeSignature = this

        override def duplicate(
          keepLocations: Boolean   = true,
          keepMetadata: Boolean    = true,
          keepDiagnostics: Boolean = true
        ): TypeSignature =
          copy(
            ir = ir.duplicate(keepLocations, keepMetadata, keepDiagnostics),
            passData =
              if (keepMetadata) passData.duplicate else MetadataStorage(),
            diagnostics =
              if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
            id = randomId
          )

        override def children: List[IR] = List(ir)

        override def showCode(indent: Int): String =
          s"(Unexpected.TypeSignature ${ir.showCode(indent)})"
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

  /** Adds extension methods for working with lists of [[IR]].
    *
    * @param list the list
    * @tparam T the concrete IR type
    */
  implicit class ListAsIr[T <: IR](list: List[T]) {

    /** Calls [[IR.duplicate]] on the elements in [[list]].
      *
      * @param keepLocations whether or not locations should be kept in the
      *                      duplicated IR
      * @param keepMetadata whether or not the pass metadata should be kept in
      *                     the duplicated IR
      * @param keepDiagnostics whether or not the diagnostics should be kept in
      *                        the duplicated IR
      * @return a duplicate of [[list]]
      */
    def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true
    ): List[T] = {
      list
        .map(_.duplicate(keepLocations, keepMetadata, keepDiagnostics))
        .asInstanceOf[List[T]]
    }
  }
}
