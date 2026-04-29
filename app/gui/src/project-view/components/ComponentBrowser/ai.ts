import { useGraphStore, useProjectNames } from '$/components/WithCurrentProject.vue'
import type { GraphStore } from '$/providers/openedProjects/graph'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import type {
  AiComponentRequest,
  AiComponentResponse,
  AiInScopeBinding,
  RequestUsage,
} from 'enso-common/src/ai'
import { Err, Ok, withContext, type Result } from 'enso-common/src/utilities/data/result'

/**
 * Resolves Component Browser AI prompts by invoking the local Claude agent hosted in
 * the Electron main process over IPC. The agent generates a top-level User Defined
 * Component plus a call placed in the current method.
 */
export function useAI(
  graphStore: GraphStore = useGraphStore(),
  projectNames: ProjectNameStore = useProjectNames(),
) {
  function buildContext(
    sourceIdentifier: string | undefined,
  ): Result<AiComponentRequest['context']> {
    const graphDb = graphStore.db
    if (sourceIdentifier != null && !graphDb.getIdentDefiningNode(sourceIdentifier)) {
      return Err(`Cannot find node with name ${sourceIdentifier}`)
    }
    const sourceTypeInfo =
      sourceIdentifier != null ? graphDb.getTypeOfIdentifier(sourceIdentifier) : null
    const sourceTypeName =
      sourceTypeInfo != null ? projectNames.printProjectPath(sourceTypeInfo.primaryType) : undefined

    if (!graphStore.currentMethod.pointer.ok) {
      return Err('Cannot determine the current method.')
    }
    if (!graphStore.currentMethod.ast.ok) {
      return Err('The current method has no parsed AST.')
    }
    const currentMethodAst = graphStore.currentMethod.ast.value
    const currentMethodName = graphStore.currentMethod.pointer.value.name
    const currentMethodCode = currentMethodAst.code()

    const inScopeBindings: AiInScopeBinding[] = []
    for (const [, ports] of graphDb.nodeOutputPorts.allForward()) {
      for (const portId of ports) {
        const identifier = graphDb.getOutputPortIdentifier(portId)
        if (identifier == null || identifier === sourceIdentifier) continue
        const typeInfo = graphDb.getTypeOfIdentifier(identifier)
        const typeName =
          typeInfo != null ? projectNames.printProjectPath(typeInfo.primaryType) : undefined
        inScopeBindings.push(typeName != null ? { identifier, typeName } : { identifier })
      }
    }

    return Ok({
      ...(sourceIdentifier != null ? { sourceIdentifier } : {}),
      ...(sourceTypeName != null ? { sourceTypeName } : {}),
      currentMethodName,
      currentMethodCode,
      inScopeBindings,
    })
  }

  async function query(
    prompt: string,
    sourceIdentifier: string | undefined,
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
        const context = buildContext(sourceIdentifier)
        if (!context.ok) return context
        const reply = await electronApi.ai.generateComponent({ prompt, context: context.value })
        logUsage(reply.usage)
        // Electron IPC uses structured clone, which strips the `ResultError` prototype —
        // `reply.result.error` comes back as a plain `{ payload, context }` object without its
        // `.message()` method. Rebuild a proper `Result` on this side of the boundary.
        return reply.result.ok ? Ok(reply.result.value) : Err(reply.result.error.payload)
      },
    )
  }

  return {
    query,
  }
}

function logUsage(usage: RequestUsage | null): void {
  if (!usage) return
  const contextKB = (usage.contextBytes / 1024).toFixed(1)
  console.log(
    `[AI] usage: prompt=${usage.inputTokens}t out=${usage.outputTokens}t context=${contextKB}kB`,
  )
}
