import { useGraphStore, useProjectNames } from '$/components/WithCurrentProject.vue'
import type { GraphStore } from '$/providers/openedProjects/graph'
import type { ProjectNameStore } from '$/providers/openedProjects/projectNames'
import { Ast } from '@/util/ast'
import type {
  AiComponentRequest,
  AiComponentResponse,
  AiInScopeBinding,
  RequestUsage,
} from 'enso-common/src/ai'
import { Err, Ok, withContext, type Result } from 'enso-common/src/utilities/data/result'

/**
 * Resolves an AI component prompt by invoking the local Claude agent in the Electron main
 * process. `dispatch()` is one IPC round-trip; the renderer-side queue
 * (`stores/ongoingAiPrompts.ts`) owns scheduling, placeholders, cancellation, and the
 * `requestId` that threads through `aiProgress` events and the cancel channel.
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

    const moduleImports: string[] = []
    const moduleRoot = currentMethodAst.module.root()
    if (moduleRoot instanceof Ast.BodyBlock) {
      for (const statement of moduleRoot.statements()) {
        if (statement instanceof Ast.Import) moduleImports.push(statement.code())
      }
    }

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
      moduleImports,
    })
  }

  async function dispatch(
    prompt: string,
    sourceIdentifier: string | undefined,
    requestId: string,
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
        const reply = await electronApi.ai.generateComponent({
          requestId,
          prompt,
          context: context.value,
        })
        logUsage(reply.usage)
        // Electron's structured clone strips the `ResultError` prototype, so rebuild it here.
        return reply.result.ok ? Ok(reply.result.value) : Err(reply.result.error.payload)
      },
    )
  }

  return {
    dispatch,
  }
}

function logUsage(usage: RequestUsage | null): void {
  if (!usage) return
  // `context=` is the last-hop prompt size (the synthesis call's actual context occupancy);
  // `hops=` and the cache breakdown that follow are turn totals from `result.usage`, useful
  // for cost analysis and for sanity-checking why a heavy-hops turn cost what it did.
  // `ctxSrc=` marks whether `context=` came from the last assistant envelope (`lastHop`) or
  // had to fall back to the cost-side sum (`fallback`). Metrics scrapers use this flag to
  // refuse writing rows for broken turns (see `aiMetrics.appendMetricsRow`).
  const contextKt = (usage.contextTokens / 1000).toFixed(1)
  const ctxSrc = usage.contextFromLastHop ? 'lastHop' : 'fallback'
  console.log(
    `[AI] usage: prompt=${usage.inputTokens}t out=${usage.outputTokens}t context=${contextKt}k hops=${usage.hopCount} ctxSrc=${ctxSrc} (cacheRead=${usage.cacheReadTokens}t cacheCreate=${usage.cacheCreationTokens}t) time=${usage.durationMs}ms`,
  )
  if (!usage.contextFromLastHop && usage.hopCount > 0) {
    console.warn(
      `[AI] WARN: contextTokens fell back to result.usage sum because the final assistant ` +
        `envelope of this turn carried no per-hop \`message.usage\` (hopCount=${usage.hopCount}). ` +
        `The reported context size is the cost-side sum, not actual context-window occupancy.`,
    )
  }
}
