import { useGraphStore } from '@/stores/graph'
import type { GraphDb } from '@/stores/graph/graphDatabase'
import { useProjectStore } from '@/stores/project'
import type { AstId } from 'shared/ast'
import type { LanguageServer } from 'shared/languageServer'
import { Err, Ok, withContext, type Result } from 'shared/util/data/result'
import type { ExternalId } from 'shared/yjsModel'

const AI_GOAL_PLACEHOLDER = '__$$GOAL$$__'
const AI_STOP_SEQUENCE = '`'

export function useAI(
  graphDb: GraphDb = useGraphStore().db,
  project: {
    lsRpcConnection: Promise<LanguageServer>
    executeExpression(expressionId: ExternalId, expression: string): Promise<Result<string> | null>
  } = useProjectStore(),
) {
  async function query(query: string, sourceIdentifier: string): Promise<Result<string>> {
    const lsRpc = await project.lsRpcConnection
    const sourceNodeId = graphDb.getIdentDefiningNode(sourceIdentifier)
    const contextId = sourceNodeId && graphDb.nodeIdToNode.get(sourceNodeId)?.outerExpr.externalId
    if (!contextId) return Err(`Cannot find node with name ${sourceIdentifier}`)

    const prompt = await project.executeExpression(
      contextId,
      `Standard.Visualization.AI.build_ai_prompt ${sourceIdentifier}`,
    )
    if (!prompt) return Err('No data from AI visualization')
    if (!prompt.ok)
      return withContext(
        () => 'When building AI propt',
        () => prompt,
      )
    const promptWithGoal = prompt.value.replace(AI_GOAL_PLACEHOLDER, query)
    if (!prompt.ok) return prompt
    try {
      const { code } = await lsRpc.aiCompletion(promptWithGoal, AI_STOP_SEQUENCE)
      return Ok(code)
    } catch (err) {
      return Err(`Error when getting AI completion: ${err}`)
    }
  }

  return {
    query,
  }
}
