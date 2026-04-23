import { useGraphStore, useProjectNames } from '$/components/WithCurrentProject.vue'
import type { GraphDb } from '$/providers/openedProjects/graph/graphDatabase'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import type { AiComponentResponse } from 'enso-common/src/ai'
import { Err, withContext, type Result } from 'enso-common/src/utilities/data/result'

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
        const sourceNodeId = graphDb.getIdentDefiningNode(sourceIdentifier)
        if (!sourceNodeId) {
          return Err(`Cannot find node with name ${sourceIdentifier}`)
        }
        const typeInfo = graphDb.getExpressionInfo(sourceNodeId)?.typeInfo
        const sourceTypeName =
          typeInfo != null ? projectNames.printProjectPath(typeInfo.primaryType) : undefined
        return electronApi.ai.generateComponent({
          prompt,
          context: {
            sourceIdentifier,
            ...(sourceTypeName != null ? { sourceTypeName } : {}),
          },
        })
      },
    )
  }

  return {
    query,
  }
}
