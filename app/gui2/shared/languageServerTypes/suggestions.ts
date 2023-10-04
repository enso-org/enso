import type { Uuid } from 'shared/languageServerTypes'

export type SuggestionId = number

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

// A type of suggestion entries.
export type SuggestionEntry =
  // A module
  | suggestionEntryVariant.Module
  // A type
  | suggestionEntryVariant.Type
  // A type constructor
  | suggestionEntryVariant.Constructor
  // A method defined on a type
  | suggestionEntryVariant.Method
  // A function
  | suggestionEntryVariant.Function
  // A local value
  | suggestionEntryVariant.Local

namespace suggestionEntryVariant {
  export interface Module {
    type: 'module'
    /** The fully qualified module name. */
    module: string

    /** The documentation string. */
    documentation?: string

    /** The fully qualified module name re-exporting this module. */
    reexport?: string
  }

  export interface Type {
    type: 'type'
    /** The external id. */
    externalId?: Uuid

    /** The type name. */
    name: string

    /** The qualified module name where the type is defined. */
    module: string

    /** The list of type parameters. */
    params: SuggestionEntryArgument[]

    /** Qualified name of the parent type. */
    parentType?: string

    /** The fully qualified module name re-exporting this type. */
    reexport?: string

    /** The documentation string. */
    documentation?: string
  }

  export interface Constructor {
    type: 'constructor'
    /** The external id. */
    externalId?: Uuid

    /** The constructor name. */
    name: string

    /** The qualified module name where this constructor is defined. */
    module: string

    /** The list of arguments. */
    arguments: SuggestionEntryArgument[]

    /** The type of the constructor. */
    returnType: string

    /** The fully qualified module name re-exporting this constructor. */
    reexport?: string

    /** The documentation string. */
    documentation?: string

    /** The list of annotations. */
    annotations: string[]
  }

  export interface Method {
    type: 'method'
    /** The external id. */
    externalId?: Uuid

    /** The method name. */
    name: string

    /** The module name where this method is defined. */
    module: string

    /** The list of arguments. */
    arguments: SuggestionEntryArgument[]

    /** The method self type. */
    selfType: string

    /** The return type of this method. */
    returnType: string

    /** The flag indicating whether this method is static or instance. */
    isStatic: boolean

    /** The fully qualified module name re-exporting this method. */
    reexport?: string

    /** The documentation string. */
    documentation?: string

    /** The list of annotations. */
    annotations: string[]
  }

  export interface Function {
    type: 'function'
    /** The external id. */
    externalId?: Uuid

    /** The function name. */
    name: string

    /** The module name where this function is defined. */
    module: string

    /** The list of arguments. */
    arguments: SuggestionEntryArgument[]

    /** The function return type. */
    returnType: string

    /** The scope where the function is defined. */
    scope: SuggestionEntryScope

    /** The documentation string. */
    documentation?: string
  }

  export interface Local {
    type: 'local'
    /** The external id. */
    externalId?: Uuid

    /** The name of a value. */
    name: string

    /** The module where this value is defined. */
    module: string

    /** The type of a value. */
    returnType: string

    /** The scope where the value is defined. */
    scope: SuggestionEntryScope

    /** The documentation string. */
    documentation?: string
  }
}

export interface SuggestionsDatabaseEntry {
  /**
   * The suggestion entry id.
   */
  id: SuggestionId

  /**
   * The suggestion entry.
   */
  suggestion: SuggestionEntry
}

type FieldAction = 'Remove' | 'Set'

export interface FieldUpdate<T> {
  /**
   * The modifying action.
   */
  tag: FieldAction

  /**
   * The updated value.
   */
  value?: T
}

export type SuggestionArgumentUpdate =
  | suggestionArgumentUpdateVariant.Add
  | suggestionArgumentUpdateVariant.Remove
  | suggestionArgumentUpdateVariant.Modify

namespace suggestionArgumentUpdateVariant {
  export interface Add {
    type: 'Add'
    /**
     * The position of the argument.
     */
    index: number

    /**
     * The argument to add.
     */
    argument: SuggestionEntryArgument
  }

  export interface Remove {
    type: 'Remove'
    /**
     * The position of the argument.
     */
    index: number
  }

  export interface Modify {
    type: 'Modify'
    /**
     * The position of the argument.
     */
    index: number

    /**
     * The name to update.
     */
    name?: FieldUpdate<string>

    /**
     * The argument type to update.
     */
    reprType?: FieldUpdate<string>

    /**
     * The isSuspended flag to update.
     */
    isSuspended?: FieldUpdate<boolean>

    /**
     * The hasDefault flag to update.
     */
    hasDefault?: FieldUpdate<boolean>

    /**
     * The default value to update.
     */
    defaultValue?: FieldUpdate<string>
  }
}

export type SuggestionsDatabaseUpdate =
  | suggestionDatabaseUpdateVariant.Add
  | suggestionDatabaseUpdateVariant.Remove
  | suggestionDatabaseUpdateVariant.Modify

namespace suggestionDatabaseUpdateVariant {
  export interface Add {
    type: 'Add'
    /**
     * Suggestion entry id.
     */
    id: SuggestionId

    /**
     * Suggestion entry.
     */
    suggestion: SuggestionEntry
  }

  export interface Remove {
    type: 'Remove'
    /**
     * Suggestion entry id.
     */
    id: SuggestionId
  }

  export interface Modify {
    type: 'Modify'
    /**
     * Suggestion entry id.
     */
    id: SuggestionId

    /**
     * The external id to update.
     */
    externalId?: FieldUpdate<Uuid>

    /**
     * The list of argument updates.
     */
    arguments?: SuggestionArgumentUpdate[]

    /**
     * The module name to update.
     */
    module?: FieldUpdate<string>

    /**
     * The self type to update.
     */
    selfType?: FieldUpdate<string>

    /**
     * The return type to update.
     */
    returnType?: FieldUpdate<string>

    /**
     * The documentation string to update.
     */
    documentation?: FieldUpdate<string>

    /**
     * The scope to update.
     */
    scope?: FieldUpdate<SuggestionEntryScope>

    /**
     * The reexport field to update.
     */
    reexport?: FieldUpdate<string>
  }
}
