import type { Uuid } from './yjsModel'

/** Version checksum of a text file - Sha3_224 */
declare const brandChecksum: unique symbol
export type Checksum = string & { [brandChecksum]: never }
export type ContextId = Uuid
export type ExpressionId = Uuid

export type ContentRoot =
  | { type: 'Project'; id: Uuid }
  | { type: 'FileSystemRoot'; id: Uuid; path: string }
  | { type: 'Home'; id: Uuid }
  | { type: 'Library'; id: Uuid; namespace: string; name: string; version: string }
  | { type: 'Custom'; id: Uuid }

/** A path is a representation of a path relative to a specified content root. */
export interface Path {
  /**  Path's root id. */
  rootId: Uuid
  /** Path's segments. */
  segments: string[]
}

export interface FileEdit {
  path: Path
  edits: TextEdit[]
  oldVersion: Checksum
  newVersion: Checksum
}

export interface TextEdit {
  range: TextRange
  text: string
}

export interface TextRange {
  start: Position
  end: Position
}

export interface Position {
  line: number
  character: number
}

export type RegisterOptions = { path: Path } | { contextId: ContextId } | {}

export interface CapabilityRegistration {
  method: string
  register_options: RegisterOptions
}

export type FileEventKind = 'Added' | 'Removed' | 'Modified'

export interface MethodCall {
  /** The method pointer of a call. */
  methodPointer: MethodPointer

  /** Indexes of arguments that have not been applied to this method. */
  notAppliedArguments: number[]
}

export type ExpressionUpdatePayload = Value | DataflowError | Panic | Pending

/**
 * Indicates that the expression was computed to a value.
 */
export interface Value {
  /**
   * Information about attached warnings.
   */
  warnings?: Warnings

  /**
   * The schema of returned function value.
   */
  functionSchema?: FunctionSchema
}

/**
 * Indicates that the expression was computed to an error.
 */
export interface DataflowError {
  /**
   * The list of expressions leading to the root error.
   */
  trace: ExpressionId[]
}

/**
 * Indicates that the expression failed with the runtime exception.
 */
export interface Panic {
  /**
   * The error message.
   */
  message: string

  /**
   * The stack trace.
   */
  trace: ExpressionId[]
}

/**
 * Indicates the expression is currently being computed. Optionally it
 * provides description and percentage (`0.0-1.0`) of completeness.
 */
export interface Pending {
  /**
   * Optional message describing current operation.
   */
  message?: string

  /**
   * Optional amount of already done work as a number between `0.0` to `1.0`.
   */
  progress?: Number
}

/**
 * Information about warnings associated with the value.
 */
export interface Warnings {
  /**
   * The number of attached warnings.
   */
  count: number

  /**
   * If the value has a single warning attached, this field contains textual
   * representation of the attached warning. In general, warning values should
   * be obtained by attaching an appropriate visualization to a value.
   */
  value?: string
}

/**
 * Contains a method pointer with information on the partially applied argument
 * positions.
 */
export interface FunctionSchema {
  /** The method pointer of this function. */
  methodPointer: MethodPointer
  /** Indexes of arguments that have not been applied to this function. */
  notAppliedArguments: number[]
}

export interface MethodPointer {
  /** The fully qualified module name. */
  module: String
  /** The type on which the method is defined. */
  definedOnType: String
  /** The method name. */
  name: String
}

export type ProfilingInfo = ExecutionTime

interface ExecutionTime {
  /** The time elapsed during the expression's evaluation, in nanoseconds */
  nanoTime: Number
}

interface ExpressionUpdate {
  /** The id of updated expression. */
  expressionId: ExpressionId
  /** The updated type of the expression. */
  type?: String
  /** The updated method call info. */
  methodCall?: MethodCall
  /** Profiling information about the expression. */
  profilingInfo: ProfilingInfo[]
  /** Wether or not the expression's value came from the cache. */
  fromCache: boolean
  /** An extra information about the computed value. */
  payload: ExpressionUpdatePayload
}

interface StackTraceElement {
  functionName: string
  path?: Path
  location?: TextRange
}

type DiagnosticType = 'Error' | 'Warning'

interface Diagnostic {
  /**
   * The type of diagnostic message.
   */
  kind: DiagnosticType

  /**
   * The diagnostic message.
   */
  message: String

  /**
   * The location of a file containing the diagnostic.
   */
  path?: Path

  /**
   * The location of the diagnostic object in a file.
   */
  location?: Range

  /**
   * The id of related expression.
   */
  expressionId?: ExpressionId

  /**
   * The stack trace.
   */
  stack: StackTraceElement[]
}

/** A representation of what kind of type a filesystem object can be. */
type FileSystemObject =
  | {
      type: 'Directory'
      name: string
      path: Path
    }
  /**
   * A directory which contents have been truncated, i.e. with its subtree not listed any further
   * due to depth limit being reached.
   */
  | {
      type: 'DirectoryTruncated'
      name: string
      path: Path
    }
  | {
      type: 'File'
      name: string
      path: Path
    }
  /** Represents other, potentially unrecognized object. Example is a broken symbolic link. */
  | {
      type: 'Other'
      name: string
      path: Path
    }
  /** Represents a symbolic link that creates a loop. */
  | {
      type: 'SymlinkLoop'
      name: string
      path: Path
      /** A target of the symlink. Since it is a loop, target is a subpath of the symlink. */
      target: Path
    }

interface VisualizationContext {}

export type Notifications = {
  'file/event': [{ path: Path; kind: FileEventKind }]
  'text/autoSave': [{ path: Path }]
  'text/didChange': [{ edits: FileEdit[] }]
  'text/fileModifiedOnDisk': [{ path: Path }]
  'executionContext/expressionUpdates': [{ contextId: ContextId; updates: ExpressionUpdate[] }]
  'executionContext/executionFailed': [{ contextId: ContextId; message: string }]
  'executionContext/executionComplete': [{ contextId: ContextId }]
  'executionContext/executionStatus': [{ contextId: ContextId; diagnostics: Diagnostic[] }]
  'search/suggestionsDatabaseUpdate': [{}]
  'file/rootAdded': [{}]
  'file/rootRemoved': [{}]
  'executionContext/visualizationEvaluationFailed': [
    {
      contextId: ContextId
      visualizationId: Uuid
      expressionId: ExpressionId
      message: String
      diagnostic?: Diagnostic
    },
  ]
  'refactoring/projectRenamed': [{}]
}

export namespace response {
  export interface OpenTextFile {
    writeCapability: CapabilityRegistration | null
    content: string
    currentVersion: Checksum
  }

  export interface InitProtocolConnection {
    contentRoots: ContentRoot[]
  }

  export interface FileList {
    paths: FileSystemObject[]
  }

  export interface VisualizationUpdate {
    context: VisualizationContext
    data: Uint8Array
  }
}
