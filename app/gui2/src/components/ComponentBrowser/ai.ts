import { useGraphStore, type Node } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { Err, Ok, type Result } from 'shared/util/data/result'

const AI_GOAL_PLACEHOLDER = '__$$GOAL$$__'
const AI_STOP_SEQUENCE = '`'

export function useAI() {
  const project = useProjectStore()

  async function query(query: string, sourceNode: Node): Promise<Result<string>> {
    const lsRpc = await project.lsRpcConnection
    const contextId = sourceNode.outerExpr.externalId
    const sourceExpression = sourceNode.pattern?.code()
    if (!sourceExpression) return Err('Cannot make AI prompt without source node.')
    const prompt = await project.executeExpression(
      contextId,
      `(${sourceExpression}).build_ai_prompt`,
    )
    if (!prompt) return Err('No data from AI visualization')
    if (!prompt.ok) return prompt
    const promptWithGoal = prompt.value.replace(AI_GOAL_PLACEHOLDER, query)
    const { code } = await lsRpc.aiCompletion(promptWithGoal, AI_STOP_SEQUENCE)
    return Ok(code)
  }

  return {
    query,
  }
}
