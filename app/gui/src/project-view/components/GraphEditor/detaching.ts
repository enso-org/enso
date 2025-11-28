import type { GraphDb, NodeId } from '$/providers/openedProjects/graph/graphDatabase'
import { set } from 'lib0'
import { isIdentifier, type AstId, type Identifier } from 'ydoc-shared/ast'

const none: unique symbol = Symbol()
type None = typeof none

/**
 * Return all changes to port values for "connecting around" operation of given selected nodes.
 *
 * The connections going out of the `selected` group will be reconnected to their self-port
 * input connection. If any connection could not be subsituted this way (e.g. no connection to self-port),
 * an empty array will be returned.
 */
export function analyzeConnectAround(selected: Set<NodeId>, graphDb: GraphDb) {
  const knownMainSourceIdentifier = new Map<NodeId, Identifier | None>()
  const result: { port: AstId; ident: Identifier }[] = []

  const findMainSourceOf = (selectedNode: NodeId) => {
    const findResult = (): Identifier | None => {
      const alreadyKnown = knownMainSourceIdentifier.get(selectedNode)
      if (alreadyKnown) return alreadyKnown
      const selfPort = graphDb.nodeIdToNode.get(selectedNode)?.primaryApplication.selfArgument
      if (!selfPort) return none
      const mainSourceId = set.first(graphDb.connections.reverseLookup(selfPort))
      if (!mainSourceId) return none
      const mainSourceNode = graphDb.getPatternExpressionNodeId(mainSourceId)
      if (mainSourceNode && selected.has(mainSourceNode)) {
        return findMainSourceOf(mainSourceNode)
      } else {
        const ident = graphDb.getOutputPortIdentifier(mainSourceId)
        return ident != null && isIdentifier(ident) ? ident : none
      }
    }
    const result = findResult()
    knownMainSourceIdentifier.set(selectedNode, result)
    return result
  }

  for (const [source, targets] of graphDb.connections.allForward()) {
    for (const target of targets) {
      // TODO[ao]: copied from collapsing.ts. Worth merging?
      const targetNode = graphDb.getExpressionNodeId(target)
      if (targetNode == null) continue
      const sourceNode = graphDb.getPatternExpressionNodeId(source)
      if (sourceNode == null) continue
      // Sometimes the connection source is in expression, not pattern; for example, when its
      // lambda.
      const nodeWithSource = sourceNode ?? graphDb.getExpressionNodeId(source)
      // If source is not in pattern nor expression of any node, it's a function argument.
      const startsInside = nodeWithSource != null && selected.has(nodeWithSource)
      const endsInside = selected.has(targetNode)
      if (startsInside && !endsInside) {
        const mainSource = findMainSourceOf(sourceNode)
        if (mainSource == none) {
          // Do not allow the action if any port would miss its source.
          return []
        }
        result.push({ port: target, ident: mainSource })
      }
    }
  }
  return result
}
