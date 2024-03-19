import { useGraphStore, type Node } from '@/stores/graph'
import { useProjectStore } from '@/stores/project'
import { Err, Ok, type Result } from 'shared/util/data/result'

const AI_GOAL_PLACEHOLDER = '__$$GOAL$$__'
const AI_STOP_SEQUENCE = '`'

export function useAI() {
  const project = useProjectStore()

  async function query(query: string, sourceNode: Node): Promise<Result<string>> {
    console.log('query:', query)
    const lsRpc = await project.lsRpcConnection
    const contextId = sourceNode.outerExpr.externalId
    const sourceExpression = sourceNode.pattern?.code()
    if (!sourceExpression) return Err('Cannot make AI prompt without source node.')
    const promptTemplate = await project.executeExpression(
      contextId,
      `(${sourceExpression}).build_ai_prompt`,
    )
    if (!promptTemplate) return Err('No data from AI visualization')
    if (!promptTemplate.ok) return promptTemplate
    const prompt = promptTemplate.value.replace(AI_GOAL_PLACEHOLDER, query)
    console.log('Prompt for the AI: ', prompt)
    const { code } = await lsRpc.aiCompletion(prompt, AI_STOP_SEQUENCE)
    console.log('Code from AI: ', code)
    return Ok(code)
  }

  return {
    query,
  }
}
