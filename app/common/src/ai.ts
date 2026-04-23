/** @file Shared IPC types for the local Claude agent that generates AI-driven components. */

/** Runtime context the renderer attaches to each AI component request. */
export interface AiComponentContext {
  /** Identifier of the source binding the generated function operates on. */
  readonly sourceIdentifier: string
  /** Fully-qualified Enso type name of the source binding, if known. */
  readonly sourceTypeName?: string
}

/** Payload sent from the renderer to the Electron main process. */
export interface AiComponentRequest {
  readonly prompt: string
  readonly context: AiComponentContext
}

/** Successful agent response. */
export interface AiComponentResponse {
  /** Enso source lines forming the body of the generated User Defined Component. */
  readonly body: string
}
