import { useGraphStore, useProjectNames } from '$/components/WithCurrentProject.vue'
import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import type { AiComponentResponse } from 'enso-common/src/ai'
import { Err, Ok, withContext, type Result } from 'enso-common/src/utilities/data/result'

/**
 * Resolves Component Browser AI prompts by invoking the local Claude agent hosted in
 * the Electron main process over IPC. The agent currently produces the body of a single
 * User Defined Component that operates on the source node supplied by the caller.
 */
export function useAI(
  graphDb: GraphDb = useGraphStore().db,
  projectNames: ProjectNameStore = useProjectNames(),
) {
  async function query(
    prompt: string,
    sourceIdentifier: string,
  ): Promise<Result<AiComponentResponse>> {
    return withContext(
      () => 'When running the AI component generator',
      async () => {
        const electronApi = typeof window === 'undefined' ? undefined : window.api
        if (!electronApi) {
          return Err(
            'AI component generation requires the desktop runtime (window.api is unavailable).',
          )
        }
        if (!graphDb.getIdentDefiningNode(sourceIdentifier)) {
          return Err(`Cannot find node with name ${sourceIdentifier}`)
        }
        const typeInfo = graphDb.getTypeOfIdentifier(sourceIdentifier)
        const sourceTypeName =
          typeInfo != null ? projectNames.printProjectPath(typeInfo.primaryType) : undefined
        const raw = await electronApi.ai.generateComponent({
          prompt,
          context: {
            sourceIdentifier,
            ...(sourceTypeName != null ? { sourceTypeName } : {}),
          },
        })
        // Electron IPC uses structured clone, which strips the `ResultError` prototype —
        // `raw.error` comes back as a plain `{ payload, context }` object without its
        // `.message()` method. Rebuild a proper `Result` on this side of the boundary.
        return raw.ok ? Ok(raw.value) : Err(raw.error.payload)
      },
    )
  }

  return {
    query,
  }
}
