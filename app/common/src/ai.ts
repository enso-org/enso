/** @file Shared IPC types for the local Claude agent that generates AI-driven components. */

import { z } from 'zod'

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

/**
 * Schema for the agent's response body. Defined with zod because this payload crosses a
 * trust boundary (it's decoded from the CLI's stdout); the request types above are
 * assembled in our own code and don't need runtime validation.
 */
export const aiComponentResponseSchema = z.object({
  /** Enso source lines forming the body of the generated User Defined Component. */
  body: z.string(),
})
export type AiComponentResponse = z.infer<typeof aiComponentResponseSchema>
