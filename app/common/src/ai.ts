/** @file Shared IPC types for the local Claude agent that generates AI-driven components. */

import { z } from 'zod'

/**
 * One identifier visible in the current method's scope, with the type the engine inferred for it
 * (when known). Used to tell the agent which existing bindings it may reference in the generated
 * function call.
 */
export interface AiInScopeBinding {
  readonly identifier: string
  readonly typeName?: string
}

/** Runtime context the renderer attaches to each AI component request. */
export interface AiComponentContext {
  /** Identifier of the source binding the user dropped into the AI prompt. */
  readonly sourceIdentifier: string
  /** Fully-qualified Enso type name of the source binding, if known. */
  readonly sourceTypeName?: string
  /** Name of the method the new node will be inserted into. */
  readonly currentMethodName: string
  /** Source code of the current method, including the signature line. */
  readonly currentMethodCode: string
  /**
   * Other bindings already defined in the current method that the agent may reference. Excludes
   * the source binding (which is reported separately as `sourceIdentifier`/`sourceTypeName`).
   */
  readonly inScopeBindings: readonly AiInScopeBinding[]
}

/** Payload sent from the renderer to the Electron main process. */
export interface AiComponentRequest {
  readonly prompt: string
  readonly context: AiComponentContext
}

/**
 * Schema for the agent's response. Defined with zod because this payload crosses a trust boundary
 * (it's decoded from the CLI's stdout); the request types above are assembled in our own code and
 * don't need runtime validation.
 *
 * The agent declares the full shape of a generated User Defined Component:
 * - `functionName` — snake_case name of the new top-level function.
 * - `argumentNames` — parameter names in the function signature (referenced by `body`). These
 *   are independent of the values passed at the call site, so the agent is free to pick names
 *   that describe the parameter's role rather than reusing whatever identifier happens to be
 *   in scope.
 * - `body` — Enso block that becomes the function body.
 * - `callArguments` — Enso expressions passed at the call site, one per parameter and in the
 *   same order as `argumentNames`. Usually plain in-scope identifiers, but any single Enso
 *   expression is allowed. The renderer assembles the actual call as
 *   `Main.<functionName> <callArguments[0]> <callArguments[1]> …`.
 */
export const aiComponentResponseSchema = z.object({
  functionName: z.string(),
  argumentNames: z.array(z.string()),
  body: z.string(),
  callArguments: z.array(z.string()),
})
export type AiComponentResponse = z.infer<typeof aiComponentResponseSchema>
