import type {
  SuggestionsDatabaseEntry,
  SuggestionsDatabaseUpdate,
} from './languageServerTypes/suggestions'
import type { ExprId, Uuid } from './yjsModel'

export type { Uuid }

/** Version checksum of a text file - Sha3_224 */
declare const brandChecksum: unique symbol
export type Checksum = string & { [brandChecksum]: never }
declare const brandContextId: unique symbol
export type ContextId = Uuid & { [brandContextId]: never }
export type ExpressionId = ExprId
declare const brandUtcDateTime: unique symbol
export type UTCDateTime = string & { [brandUtcDateTime]: never }

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

export interface FileContents<T> {
  contents: T
}

export interface TextFileContents extends FileContents<string> {}

export interface DirectoryTree {
  path: Path
  name: string
  files: FileSystemObject[]
  directories: DirectoryTree[]
}

export interface FileAttributes {
  creationTime: UTCDateTime
  lastAccessTime: UTCDateTime
  lastModifiedTime: UTCDateTime
  kind: FileSystemObject
  byteSize: number
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
  type: 'Value'
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
  type: 'DataflowError'
  /**
   * The list of expressions leading to the root error.
   */
  trace: ExpressionId[]
}

/**
 * Indicates that the expression failed with the runtime exception.
 */
export interface Panic {
  type: 'Panic'
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
  type: 'Pending'
  /** Optional message describing current operation. */
  message?: string
  /** Optional amount of already done work as a number between `0.0` to `1.0`. */
  progress?: number
}

/**
 * Information about warnings associated with the value.
 */
export interface Warnings {
  /** The number of attached warnings. */
  count: number
  /** If the value has a single warning attached, this field contains textual
   * representation of the attached warning. In general, warning values should
   * be obtained by attaching an appropriate visualization to a value. */
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
  module: string
  /** The type on which the method is defined. */
  definedOnType: string
  /** The method name. */
  name: string
}

export type ProfilingInfo = ExecutionTime

export interface ExecutionTime {
  /** The time elapsed during the expression's evaluation, in nanoseconds */
  nanoTime: number
}

export interface ExpressionUpdate {
  /** The id of updated expression. */
  expressionId: ExpressionId
  /** The updated type of the expression. */
  type?: string
  /** The updated method call info. */
  methodCall?: MethodCall
  /** Profiling information about the expression. */
  profilingInfo: ProfilingInfo[]
  /** Wether or not the expression's value came from the cache. */
  fromCache: boolean
  /** An extra information about the computed value. */
  payload: ExpressionUpdatePayload
}

export interface StackTraceElement {
  functionName: string
  path?: Path
  location?: TextRange
}

export type DiagnosticType = 'Error' | 'Warning'

export interface Diagnostic {
  /** The type of diagnostic message. */
  kind: DiagnosticType
  /** The diagnostic message. */
  message: string
  /** The location of a file containing the diagnostic. */
  path?: Path
  /** The location of the diagnostic object in a file. */
  location?: TextRange
  /** The id of related expression. */
  expressionId?: ExpressionId
  /** The stack trace. */
  stack: StackTraceElement[]
}

/** A representation of what kind of type a filesystem object can be. */
export type FileSystemObject =
  | {
      type: 'Directory'
      name: string
      path: Path
    }
  /** A directory which contents have been truncated, i.e. with its subtree not listed any further
   * due to depth limit being reached. */
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

export interface VisualizationConfiguration {
  /** An execution context of the visualization. */
  executionContextId: ContextId
  /** A qualified name of the module to be used to evaluate the arguments for the visualization
   * expression. */
  visualizationModule: string
  /** An expression that creates a visualization. */
  expression: string | MethodPointer
  /** A list of arguments to pass to the visualization expression. */
  positionalArgumentsExpressions?: string[]
}

export interface VCSSave {
  commitId: string
  message: string
}

export type Notifications = {
  'text/autoSave': (param: { path: Path }) => void
  'text/didChange': (param: { edits: FileEdit[] }) => void
  'text/fileModifiedOnDisk': (param: { path: Path }) => void
  'executionContext/expressionUpdates': (param: {
    contextId: ContextId
    updates: ExpressionUpdate[]
  }) => void
  'executionContext/executionFailed': (param: { contextId: ContextId; message: string }) => void
  'executionContext/executionComplete': (param: { contextId: ContextId }) => void
  'executionContext/executionStatus': (param: {
    contextId: ContextId
    diagnostics: Diagnostic[]
  }) => void
  'executionContext/visualizationEvaluationFailed': (param: {
    contextId: ContextId
    visualizationId: Uuid
    expressionId: ExpressionId
    message: string
    diagnostic?: Diagnostic
  }) => void
  'search/suggestionsDatabaseUpdates': (param: {
    updates: SuggestionsDatabaseUpdate[]
    currentVersion: number
  }) => void
  'file/event': (param: { path: Path; kind: FileEventKind }) => void
  'file/rootAdded': (param: {}) => void
  'file/rootRemoved': (param: {}) => void
  'refactoring/projectRenamed': (param: {}) => void
}

export type ExecutionEnvironment = 'Design' | 'Live'

export type StackItem = ExplicitCall | LocalCall

export interface ExplicitCall {
  type: 'ExplicitCall'
  methodPointer: MethodPointer
  thisArgumentExpression?: string | undefined
  positionalArgumentsExpressions: string[]
}

export interface LocalCall {
  type: 'LocalCall'
  expressionId: ExpressionId
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

  export interface FileContents {
    contents: TextFileContents
  }

  export interface FileExists {
    exists: boolean
  }

  export interface FileTree {
    tree: DirectoryTree
  }

  export interface FileList {
    paths: FileSystemObject[]
  }

  export interface FileInfo {
    attributes: FileAttributes
  }

  export interface FileChecksum {
    checksum: Checksum
  }

  export interface VCSCommit {
    commitId: string
    message: string
  }

  export interface VCSStatus {
    dirty: boolean
    changed: Path[]
    lastSave: VCSSave
  }

  export interface VCSChanges {
    changed: Path[]
  }

  export interface VCSSaves {
    saves: VCSSave[]
  }

  export interface ExecutionContext {
    contextId: ContextId
    canModify: CapabilityRegistration
    receivesUpdates: CapabilityRegistration
  }

  export interface VisualizationUpdate {
    context: VisualizationContext
    data: Uint8Array
  }

  export interface GetSuggestionsDatabase {
    entries: SuggestionsDatabaseEntry[]
    currentVersion: number
  }
}

export interface LanguageServerError {
  code: LanguageServerErrorCode
  message: string
  payload?: Record<string, string | number> | Diagnostic
}

export enum LanguageServerErrorCode {
  // === Error API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/protocol/json/ErrorApi.scala
  /** The user doesn't have access to the requested resource.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#accessdeniederror) */
  AccessDenied = 100,

  // === VCS Manager API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/vcsmanager/VcsManagerApi.scala
  // `ContentRootNotFound` is also defined by the File Manager API with the same code, so it is omitted here.
  /** A miscellaneous VCS error. */
  VCS = 1000,
  /** The project was not found in the VCS. */
  VCSProjectNotFound = 1002,
  /** The project is not under version control. */
  VCSNotFound = 1003,
  /** The requested save could not be found. */
  SaveNotFound = 1004,
  /** The requested project is already under version control. */
  VCSAlreadyExists = 1005,

  // === File Manager API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/filemanager/FileManagerApi.scala
  /** A miscellaneous file system error.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filesystemerror) */
  FileSystem = 1000,
  /** The requested content root could not be found.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#contentrootnotfounderror) */
  ContentRootNotFound = 1001,
  /** The requested file does not exist.
   *
   *[Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filenotfound) */
  FileNotFound = 1003,
  /** The file trying to be created already exists.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#fileexists) */
  FileExists = 1004,
  /** The IO operation timed out.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#operationtimeouterror) */
  OperationTimeoutError = 1005,
  /** The provided path is not a directory.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#notdirectory) */
  NotDirectory = 1006,
  /** The provided path is not a file.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#notfile) */
  NotFile = 1007,
  /** The streaming file write cannot overwrite a portion of the requested file.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#cannotoverwrite) */
  CannotOverwrite = 1008,
  /** The requested file read was out of bounds for the file's size.
   *
   * The actual length of the file is returned in `payload.fileLength`.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#readoutofbounds) */
  ReadOutOfBounds = 1009,
  /** The project configuration cannot be decoded.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#cannotdecode) */
  CannotDecode = 1010,

  // === Execution API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/runtime/ExecutionApi.scala
  /** The provided execution stack item could not be found.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#stackitemnotfounderror) */
  StackItemNotFound = 2001,
  /** The provided exeuction context could not be found.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#contextnotfounderror) */
  ContextNotFound = 2002,
  /** The execution stack is empty.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#emptystackerror) */
  EmptyStack = 2003,
  /** The stack is invalid in this context.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#invalidstackitemerror) */
  InvalidStackItem = 2004,
  /** The provided module could not be found.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#modulenotfounderror) */
  ModuleNotFound = 2005,
  /** The provided visualization could not be found.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#visualizationnotfounderror) */
  VisualizationNotFound = 2006,
  /** The expression specified in the {@link VisualizationConfiguration} cannot be evaluated.
   *
   * If relevant, a {@link Diagnostic} containing error details is returned as `payload`.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#visualizationexpressionerror) */
  VisualizationExpression = 2007,

  // === Text API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/text/TextApi.scala
  /** A file was not opened.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#filenotopenederror) */
  FileNotOpened = 3001,
  /** Validation has failed for a series of text edits.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#texteditvalidationerror) */
  TextEditValidation = 3002,
  /** The version provided by a client does not match the version computed by the server.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#invalidversionerror) */
  InvalidVersion = 3003,
  /** The client doesn't hold write lock to the buffer.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#writedeniederror) */
  WriteDenied = 3004,

  // === Capability API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/capability/CapabilityApi.scala
  /** The requested capability is not acquired.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#accessdeniederror) */
  CapabilityNotAcquired = 5001,

  // === Session API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/session/SessionApi.scala
  /** The request could not be proccessed, beacuse the session is not initialised.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#sessionnotinitialisederror) */
  SessionNotInitialised = 6001,
  /** The session is already initialised.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#sessionalreadyinitialisederror) */
  SessionAlreadyInitialised = 6002,

  // === Search API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/search/SearchApi.scala
  /** There was an unexpected error accessing the suggestions database.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#suggestionsdatabaseerror) */
  SuggestionsDatabase = 7001,
  /** The project was not found in the root directory.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#projectnotfounderror) */
  ProjectNotFound = 7002,
  /** The module name could not be resolved for the given file.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#modulenamenotresolvederror) */
  ModuleNameNotResolved = 7003,
  /** The requested suggestion could not be found.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#suggestionnotfounderror) */
  SuggestionNotFound = 7004,

  // === Library API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/libraries/LibraryApi.scala
  /** The requested edition could not be found.
   *
   * The requested edition is returned in `payload.editionName`.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#editionnotfounderror) */
  EditionNotFound = 8001,
  /** A local library with the specified namespace and name combination already exists, so it cannot be created again.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#libraryalreadyexists) */
  LibraryAlreadyExists = 8002,
  /** Authentication to the library repository was declined.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#libraryrepositoryauthenticationerror) */
  LibraryRepositoryAuthentication = 8003,
  /** A request to the library repository failed.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#librarypublisherror) */
  LibraryPublish = 8004,
  /** Uploading the library failed for network-related reasons.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#libraryuploaderror) */
  LibraryUpload = 8005,
  /** Downloading the library failed for network-related reasons, or the library was not found in the repository.
   *
   * The requested library is returned in `payload.namespace`, `payload.name`, and `payload.version`.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#librarydownloaderror) */
  LibraryDownload = 8006,
  /** A local library with the specified namespace and name combination was not found on the local libraries path.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#locallibrarynotfound) */
  LocalLibraryNotFound = 8007,
  /** A library could not be resolved. It was not defined in the edition, and the settings did not
   * allow to resolve local libraries, or it did not exist there either.
   *
   * The requested namespace and name are returned in `payload.namespace` and `payload.name`.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#librarynotresolved) */
  LibraryNotResolved = 8008,
  /** The chosen library name is invalid.
   *
   * A similar, valid name is returned in `payload.suggestedName`.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#invalidlibraryname) */
  InvalidLibraryName = 8009,
  /** The library preinstall endpoint could not properly find dependencies of the requested library.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#dependencydiscoveryerror) */
  DependencyDiscovery = 8010,
  /** The provided version string is not a valid semver version.
   *
   * The requested version is returned in `payload.version`.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#invalidsemverversion) */
  InvalidSemverVersion = 8011,

  // === Refactoring API errors ===
  // https://github.com/enso-org/enso/blob/develop/engine/language-server/src/main/scala/org/enso/languageserver/refactoring/RefactoringApi.scala
  /** An expression with the provided ID could not be found.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#expressionnotfounderror) */
  ExpressionNotFound = 9001,
  /** The refactoring operation was not able to apply the generated edits.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#failedtoapplyedits) */
  FailedToApplyEdits = 9002,
  /** Refactoring of the given expression is not supported.
   *
   * [Documentation](https://github.com/enso-org/enso/blob/develop/docs/language-server/protocol-language-server.md#refactoringnotsupported) */
  RefactoringNotSupported = 9003,
}
