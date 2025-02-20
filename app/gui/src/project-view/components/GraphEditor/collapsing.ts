import type { GraphDb, NodeId } from '@/stores/graph/graphDatabase'
import { nodeIdFromOuterAst } from '@/stores/graph/graphDatabase'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import type { Identifier } from '@/util/ast/abstract'
import { isIdentifier, moduleMethodNames } from '@/util/ast/abstract'
import { Err, Ok, unwrap, type Result } from '@/util/data/result'
import { tryIdentifier } from '@/util/qualifiedName'
import * as set from 'lib0/set'

// === Types ===

/** Information about code transformations needed to collapse the nodes. */
interface CollapsedInfo {
  extracted: ExtractedInfo
  refactored: RefactoredInfo
}

/** The information about the extracted function. */
interface ExtractedInfo {
  /** Nodes with these ids should be moved to the function body, in their original order. */
  ids: Set<NodeId>
  /** The output information of the function. */
  output: Output
  /** The list of extracted function’s argument names. */
  inputs: Identifier[]
}

/** The information about the output value of the extracted function. */
interface Output {
  /**
   * The id of the node the expression of which should be replaced by the function call.
   * This node is also included into `ids` of the {@link ExtractedInfo} and must be moved into the extracted function.
   */
  node: NodeId
  /** The identifier of the return value of the extracted function. */
  identifier: Identifier
}

/** The information about the refactored node, the one that needs to be replaced with the function call. */
interface RefactoredInfo {
  /** The id of the refactored node. */
  id: NodeId
  /** The pattern of the refactored node. Included for convenience, collapsing does not affect it. */
  pattern: Identifier
  /** The list of necessary arguments for a call of the collapsed function. */
  arguments: Identifier[]
}

// === prepareCollapsedInfo ===

/**
 * Prepare the information necessary for collapsing nodes.
 * @throws errors in case of failures, but it should not happen in normal execution.
 */
export function prepareCollapsedInfo(
  selected: Set<NodeId>,
  graphDb: GraphDb,
): Result<CollapsedInfo> {
  if (selected.size == 0) throw new Error('Collapsing requires at least a single selected node.')
  // Leaves are the nodes that have no outgoing connection.
  const leaves = new Set(selected)
  const inputSet: Set<Identifier> = new Set()
  let output: Output | null = null
  for (const [targetExprId, sourceExprIds] of graphDb.connections.allReverse()) {
    const targetNode = graphDb.getExpressionNodeId(targetExprId)
    if (targetNode == null) continue
    for (const sourceExprId of sourceExprIds) {
      const sourceNode = graphDb.getPatternExpressionNodeId(sourceExprId)
      // Sometimes the connection source is in expression, not pattern; for example, when its
      // lambda.
      const nodeWithSource = sourceNode ?? graphDb.getExpressionNodeId(sourceExprId)
      // If source is not in pattern nor expression of any node, it's a function argument.
      const startsInside = nodeWithSource != null && selected.has(nodeWithSource)
      const endsInside = selected.has(targetNode)
      const stringIdentifier = graphDb.getOutputPortIdentifier(sourceExprId)
      if (stringIdentifier == null)
        throw new Error(`Connection starting from (${sourceExprId}) has no identifier.`)
      const identifier = unwrap(tryIdentifier(stringIdentifier))
      if (sourceNode != null) {
        leaves.delete(sourceNode)
      }
      if (!startsInside && endsInside) {
        inputSet.add(identifier)
      } else if (startsInside && !endsInside) {
        assert(sourceNode != null) // No lambda argument set inside node should be visible outside.
        if (output == null) {
          output = { node: sourceNode, identifier }
        } else if (output.identifier == identifier) {
          // Ignore duplicate usage of the same identifier.
        } else {
          return Err(
            `More than one output from collapsed function: ${identifier} and ${output.identifier}. Collapsing is not supported.`,
          )
        }
      }
    }
  }
  // If there is no output found so far, it means that none of our nodes is used outside
  // the extracted function. In such case we will return value from arbitrarily chosen leaf.
  if (output == null) {
    const arbitraryLeaf = set.first(leaves)
    if (arbitraryLeaf == null) throw Error('Cannot select the output node, no leaf nodes found.')
    const outputNode = graphDb.nodeIdToNode.get(arbitraryLeaf)
    if (outputNode == null) throw new Error(`The node with id ${arbitraryLeaf} not found.`)
    const identifier = unwrap(tryIdentifier(outputNode.pattern?.code() || ''))
    output = { node: arbitraryLeaf, identifier }
  }

  const pattern = graphDb.nodeIdToNode.get(output.node)?.pattern?.code()
  assert(pattern != null && isIdentifier(pattern))
  const inputs = Array.from(inputSet)

  assert(selected.has(output.node))
  return Ok({
    extracted: {
      ids: selected,
      output,
      inputs,
    },
    refactored: {
      id: output.node,
      pattern,
      arguments: inputs,
    },
  })
}

/** Generate a safe method name for a collapsed function using `baseName` as a prefix. */
function findSafeMethodName(topLevel: Ast.BodyBlock, baseName: Identifier): Identifier {
  const allIdentifiers = moduleMethodNames(topLevel)
  if (!allIdentifiers.has(baseName)) {
    return baseName
  }
  let index = 1
  while (allIdentifiers.has(`${baseName}${index}`)) {
    index++
  }
  const name = `${baseName}${index}`
  assert(isIdentifier(name))
  return name
}

// === performCollapse ===

// We support working inside `Main` module of the project at the moment.
const MODULE_NAME = 'Main' as Identifier
const COLLAPSED_FUNCTION_NAME = 'collapsed' as Identifier

interface CollapsingResult {
  /** The ID of the node refactored to the collapsed function call. */
  collapsedCallRoot: Ast.AstId
  /**
   * IDs of nodes inside the collapsed function, except the output node.
   * The order of these IDs is reversed comparing to the order of nodes in the source code.
   */
  collapsedNodeIds: NodeId[]
  /** ID of the output AST node inside the collapsed function. */
  outputAstId: Ast.AstId
}

interface PreparedCollapseInfo {
  args: Identifier[]
  statementsToExtract: Set<Ast.AstId>
  statementToReplace: Ast.AstId
}

/** Perform the actual AST refactoring for collapsing nodes. */
export function performCollapse(
  info: CollapsedInfo,
  topLevel: Ast.MutableBodyBlock,
  graphDb: GraphDb,
  currentMethodName: string,
): CollapsingResult {
  const nodeIdToStatementId = (nodeId: NodeId) => graphDb.nodeIdToNode.get(nodeId)!.outerAst.id
  const preparedInfo = {
    args: info.extracted.inputs,
    statementsToExtract: new Set([...info.extracted.ids].map(nodeIdToStatementId)),
    statementToReplace: nodeIdToStatementId(info.refactored.id),
    outputIdentifier: info.extracted.output.identifier,
  }
  return performCollapseImpl(topLevel, preparedInfo, currentMethodName)
}

/** @internal */
export function performCollapseImpl(
  topLevel: Ast.MutableBodyBlock,
  info: PreparedCollapseInfo,
  currentMethodName: string,
) {
  const edit = topLevel.module
  const collapsedName = findSafeMethodName(topLevel, COLLAPSED_FUNCTION_NAME)
  const { statement: currentMethod, index: currentMethodLine } = Ast.findModuleMethod(
    topLevel,
    currentMethodName,
  )!

  // Update the definition of the refactored function.
  const extractedLines = currentMethod
    .bodyAsBlock()
    .extractIf(({ id }) => info.statementsToExtract.has(id) && id !== info.statementToReplace)
  const collapsedCall = Ast.App.PositionalSequence(
    Ast.PropertyAccess.new(edit, Ast.Ident.new(edit, MODULE_NAME), collapsedName),
    info.args.map((arg) => Ast.Ident.new(edit, arg)),
  )
  const statementToReplace = edit.get(info.statementToReplace)
  assert(statementToReplace instanceof Ast.MutableAssignment)
  const outputIdentifier = statementToReplace.pattern.code() as Identifier
  extractedLines.push({
    statement: {
      whitespace: undefined,
      node: statementToReplace.replace(
        Ast.Assignment.new(outputIdentifier, collapsedCall, { edit }),
      ),
    },
  })

  const collapsedNodeIds = extractedLines
    .map(({ statement }) => statement && nodeIdFromOuterAst(statement.node))
    .filter((id) => id != null)
    .reverse()

  // Insert a new function.
  const collapsedBody = Ast.BodyBlock.new(extractedLines, edit)
  const outputAst = Ast.Ident.new(edit, outputIdentifier)
  collapsedBody.push(outputAst)
  const collapsedFunction = Ast.FunctionDef.new(collapsedName, info.args, collapsedBody, {
    edit,
    documentation: 'ICON group',
  })
  topLevel.insert(currentMethodLine, collapsedFunction, undefined)

  return { collapsedCallRoot: collapsedCall.id, outputAstId: outputAst.id, collapsedNodeIds }
}
