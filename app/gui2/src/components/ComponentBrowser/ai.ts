import { useGraphStore, type Node } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import type { AstId } from 'shared/ast'
import { Err, Ok, withContext, type Result } from 'shared/util/data/result'

const AI_GOAL_PLACEHOLDER = '__$$GOAL$$__'
const AI_STOP_SEQUENCE = '`'

export function useAI() {
  const project = useProjectStore()
  const graphDb = useGraphStore().db

  async function query(query: string, sourcePort: AstId): Promise<Result<string>> {
    const lsRpc = await project.lsRpcConnection
    const sourceNodeId = graphDb.getPatternExpressionNodeId(sourcePort)
    const contextId = sourceNodeId && graphDb.nodeIdToNode.get(sourceNodeId)?.outerExpr.externalId
    if (!contextId) return Err(`Cannot find node with source port ${sourcePort}`)
    const ident = graphDb.getOutputPortIdentifier(sourcePort)
    if (!ident) return Err(`Cannot find identifier of source port ${sourcePort}`)

    const prompt = await project.executeExpression(contextId, `(${ident}).build_ai_prompt`)
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
