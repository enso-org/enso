import type { GraphStore } from '$/providers/openedProjects/graph'
import type { NodeId } from '$/providers/openedProjects/graph/graphDatabase'
import { Err, Ok } from 'enso-common/src/utilities/data/result'
import { set } from 'lib0'
import { isIdentifier, type AstId, type Identifier } from 'ydoc-shared/ast'

/**
 * Return all changes to port values for "connecting around" operation of given selected nodes.
 *
 * The connections going out of the `selected` group will be reconnected to their self-port
 * input connection. Returns Err If any connection could not be subsituted this way (e.g. no connection to self-port).
 */
export function analyzeConnectAround(
  selected: Set<NodeId>,
  graph: Pick<GraphStore, 'db' | 'pickInCodeOrder'>,
) {
  const mainSourceIdentifier = new Map<NodeId, Identifier>()
  const result: { port: AstId; ident: Identifier }[] = []

  for (const selectedNode of graph.pickInCodeOrder(selected)) {
    const selfPort = graph.db.nodeIdToNode.get(selectedNode)?.primaryApplication.selfArgument
    if (!selfPort) continue
    const mainSourceId = set.first(graph.db.connections.reverseLookup(selfPort))
    if (!mainSourceId) continue
    const mainSourceNode = graph.db.getPatternExpressionNodeId(mainSourceId)
    if (mainSourceNode && selected.has(mainSourceNode)) {
      const ident = mainSourceIdentifier.get(mainSourceNode)
      if (ident) {
        mainSourceIdentifier.set(selectedNode, ident)
      }
    } else {
      const ident = graph.db.getOutputPortIdentifier(mainSourceId)
      if (ident != null && isIdentifier(ident)) mainSourceIdentifier.set(selectedNode, ident)
    }
  }

  for (const [source, targets] of graph.db.connections.allForward()) {
    for (const target of targets) {
      // TODO[ao]: copied from collapsing.ts. Worth merging?
      const targetNode = graph.db.getExpressionNodeId(target)
      if (targetNode == null) continue
      const sourceNode = graph.db.getPatternExpressionNodeId(source)
      if (sourceNode == null) continue
      // Sometimes the connection source is in expression, not pattern; for example, when its
      // lambda.
      const nodeWithSource = sourceNode ?? graph.db.getExpressionNodeId(source)
      // If source is not in pattern nor expression of any node, it's a function argument.
      const startsInside = nodeWithSource != null && selected.has(nodeWithSource)
      const endsInside = selected.has(targetNode)
      if (startsInside && !endsInside) {
        const mainSource = mainSourceIdentifier.get(sourceNode)
        if (mainSource == null) {
          // Do not allow the action if any port would miss its source.
          return Err(`No self-port route for port ${target}`)
        }
        result.push({ port: target, ident: mainSource })
      }
    }
  }
  return Ok(result)
}
