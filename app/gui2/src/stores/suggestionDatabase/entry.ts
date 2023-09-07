export type SuggestionId = number

export type UUID = string
export type QualifiedName = string

// The kind of a suggestion.
export enum Kind {
  Module,
  Type,
  Constructor,
  Method,
  Function,
  Local,
}

// The argument of a constructor, method or function suggestion.
export interface SuggestionEntryArgument {
  /** The argument name. */
  name: string
  /** The argument type. String 'Any' is used to specify generic types. */
  type: string
  /** Indicates whether the argument is lazy. */
  isSuspended: boolean
  /** Indicates whether the argument has default value. */
  hasDefault: boolean
  /** Optional default value. */
  defaultValue?: string
  /** Optional list of possible values that this argument takes. */
  tagValues?: string[]
}

export interface Position {
  /**
   * Line position in a document (zero-based).
   */
  line: number

  /**
   * Character offset on a line in a document (zero-based). Assuming that the
   * line is represented as a string, the `character` value represents the gap
   * between the `character` and `character + 1`.
   *
   * If the character value is greater than the line length it defaults back to
   * the line length.
   */
  character: number
}

// The definition scope
export interface SuggestionEntryScope {
  // The start position of the definition scope
  start: Position
  // The end position of the definition scope
  end: Position
}

export interface SuggestionEntry {
  kind: Kind
  /// A module where the suggested object is defined.
  definedIn: QualifiedName
  /// A type or module this method or constructor belongs to.
  memberOf?: QualifiedName
  /// A name of suggested object.
  name: string
  /// Argument lists of suggested object (atom or function). If the object does not take any
  /// arguments, the list is empty.
  arguments: SuggestionEntryArgument[]
  /// A type returned by the suggested object.
  return_type: QualifiedName
  /// A module reexporting this entity.
  reexportedIn: QualifiedName
  /// A list of documentation sections associated with object.
  documentation: string
  /// A type of the "self" argument. This field is present only for instance methods.
  selfType?: QualifiedName
  /// A scope where this suggestion is visible.
  scope: SuggestionEntryScope
  /// A name of a custom icon to use when displaying the entry.
  icon_name: string
  /// A name of a group this entry belongs to.
  group_index: number
}

// A type of suggestion entries.
// export type SuggestionEntry =
//   // A module
//   | Module
//   // A type
//   | Type
//   // A type constructor
//   | Constructor
//   // A method defined on a type
//   | Method
//   // A function
//   | Function
//   // A local value
//   | Local

// export interface Module {
//   /** The fully qualified module name. */
//   module: string

//   /** The documentation string. */
//   documentation?: string

//   /** The fully qualified module name re-exporting this module. */
//   reexport?: string
// }

// export interface Type {
//   /** The external id. */
//   externalId?: UUID

//   /** The type name. */
//   name: string

//   /** The qualified module name where the type is defined. */
//   module: string

//   /** The list of type parameters. */
//   params: SuggestionEntryArgument[]

//   /** Qualified name of the parent type. */
//   parentType?: string

//   /** The fully qualified module name re-exporting this type. */
//   reexport?: string

//   /** The documentation string. */
//   documentation?: string
// }

// export interface Constructor {
//   /** The external id. */
//   externalId?: UUID

//   /** The constructor name. */
//   name: string

//   /** The qualified module name where this constructor is defined. */
//   module: string

//   /** The list of arguments. */
//   arguments: SuggestionEntryArgument[]

//   /** The type of the constructor. */
//   returnType: string

//   /** The fully qualified module name re-exporting this constructor. */
//   reexport?: string

//   /** The documentation string. */
//   documentation?: string

//   /** The list of annotations. */
//   annotations: string[]
// }

// export interface Method {
//   /** The external id. */
//   externalId?: UUID

//   /** The method name. */
//   name: string

//   /** The module name where this method is defined. */
//   module: string

//   /** The list of arguments. */
//   arguments: SuggestionEntryArgument[]

//   /** The method self type. */
//   selfType: string

//   /** The return type of this method. */
//   returnType: string

//   /** The flag indicating whether this method is static or instance. */
//   isStatic: boolean

//   /** The fully qualified module name re-exporting this method. */
//   reexport?: string

//   /** The documentation string. */
//   documentation?: string

//   /** The list of annotations. */
//   annotations: string[]
// }

// export interface Function {
//   /** The external id. */
//   externalId?: UUID

//   /** The function name. */
//   name: string

//   /** The module name where this function is defined. */
//   module: string

//   /** The list of arguments. */
//   arguments: SuggestionEntryArgument[]

//   /** The function return type. */
//   returnType: string

//   /** The scope where the function is defined. */
//   scope: SuggestionEntryScope

//   /** The documentation string. */
//   documentation?: string
// }

// export interface Local {
//   /** The external id. */
//   externalId?: UUID

//   /** The name of a value. */
//   name: string

//   /** The module where this value is defined. */
//   module: string

//   /** The type of a value. */
//   returnType: string

//   /** The scope where the value is defined. */
//   scope: SuggestionEntryScope

//   /** The documentation string. */
//   documentation?: string
// }
