import { subtreeRoots } from '.'
import { assertDefined, assertEqual } from '../util/assert'
import { sourceRangeFromKey } from '../util/data/text'
import { type NodeKey, type NodeSpanMap } from './idMap'
import { type MutableModule } from './mutableModule'
import { parseModuleWithSpans } from './parse'
import { printWithSpans } from './print'
import { BodyBlock, Group, type Ast, type AstId } from './tree'

/**
 * Try to find all the spans in `expected` in `encountered`. If any are missing, use the provided `code` to determine
 *  whether the lost spans are single-line or multi-line.
 */
function checkSpans(expected: NodeSpanMap, encountered: NodeSpanMap, code: string) {
  const lost = new Array<readonly [NodeKey, Ast]>()
  for (const [key, asts] of expected) {
    const outermostPrinted = asts[0]
    if (!outermostPrinted) continue
    for (let i = 1; i < asts.length; ++i) assertEqual(asts[i]?.parentId, asts[i - 1]?.id)
    const encounteredAsts = encountered.get(key)
    if (encounteredAsts === undefined) lost.push([key, outermostPrinted])
  }
  const lostInline = new Array<Ast>()
  const lostBlock = new Array<Ast>()
  for (const [key, ast] of lost) {
    const { from, to } = sourceRangeFromKey(key)
    // Do not report lost empty body blocks, we don't want them to be considered for repair.
    if (from === to && ast instanceof BodyBlock) continue
    ;(code.substring(from, to).match(/[\r\n]/) ? lostBlock : lostInline).push(ast)
  }
  return { lostInline, lostBlock }
}

/**
 * If the input tree's concrete syntax has precedence errors (i.e. its expected code would not parse back to the same
 *  structure), try to fix it. If possible, it will be repaired by inserting parentheses; if that doesn't fix it, the
 *  affected subtree will be re-synced to faithfully represent the source code the incorrect tree prints to.
 */
export function repair(
  root: BodyBlock,
  module?: MutableModule,
): { code: string; fixes: MutableModule | undefined } {
  // Print the input to see what spans its nodes expect to have in the output.
  const printed = printWithSpans(root)
  // Parse the printed output to see what spans actually correspond to nodes in the printed code.
  const reparsed = parseModuleWithSpans(printed.code)
  // See if any span we expected to be a node isn't; if so, it likely merged with its parent due to wrong precedence.
  const { lostInline, lostBlock } = checkSpans(
    printed.info.nodes,
    reparsed.spans.nodes,
    printed.code,
  )
  if (lostInline.length === 0) {
    if (lostBlock.length !== 0) {
      console.warn(`repair: Bad block elements, but all inline elements OK?`)
      const fixes = module ?? root.module.edit()
      resync(lostBlock, printed.info.nodes, reparsed.spans.nodes, fixes)
      return { code: printed.code, fixes }
    }
    return { code: printed.code, fixes: undefined }
  }

  // Wrap any "lost" nodes in parentheses.
  const fixes = module ?? root.module.edit()
  for (const ast of lostInline) {
    if (ast instanceof Group) continue
    fixes.getVersion(ast).update((ast) => Group.new(fixes, ast as any))
  }

  // Verify that it's fixed.
  const printed2 = printWithSpans(fixes.root()!)
  const reparsed2 = parseModuleWithSpans(printed2.code)
  const { lostInline: lostInline2, lostBlock: lostBlock2 } = checkSpans(
    printed2.info.nodes,
    reparsed2.spans.nodes,
    printed2.code,
  )
  if (lostInline2.length !== 0 || lostBlock2.length !== 0)
    resync([...lostInline2, ...lostBlock2], printed2.info.nodes, reparsed2.spans.nodes, fixes)

  return { code: printed2.code, fixes }
}

/**
 * Replace subtrees in the module to ensure that the module contents are consistent with the module's code.
 * @param badAsts - ASTs that, if printed, would not parse to exactly their current content.
 * @param badSpans - Span map produced by printing the `badAsts` nodes and all their parents.
 * @param goodSpans - Span map produced by parsing the code from the module of `badAsts`.
 * @param edit - Module to apply the fixes to; must contain all ASTs in `badAsts`.
 */
function resync(
  badAsts: Iterable<Ast>,
  badSpans: NodeSpanMap,
  goodSpans: NodeSpanMap,
  edit: MutableModule,
) {
  const parentsOfBadSubtrees = new Set<AstId>()
  const badAstIds = new Set(Array.from(badAsts, (ast) => ast.id))
  for (const id of subtreeRoots(edit, badAstIds)) {
    const parent = edit.get(id)?.parentId
    if (parent) parentsOfBadSubtrees.add(parent)
  }

  const spanOfBadParent = new Array<readonly [AstId, NodeKey]>()
  for (const [span, asts] of badSpans) {
    for (const ast of asts) {
      if (parentsOfBadSubtrees.has(ast.id)) spanOfBadParent.push([ast.id, span])
    }
  }
  // All ASTs in the module of badAsts should have entries in badSpans.
  assertEqual(spanOfBadParent.length, parentsOfBadSubtrees.size)

  for (const [id, span] of spanOfBadParent) {
    const parent = edit.get(id)
    const goodAst = goodSpans.get(span)?.[0]
    // The parent of the root of a bad subtree must be a good AST.
    assertDefined(goodAst)
    parent.syncToCode(goodAst.code())
  }

  console.warn(
    `repair: Replaced ${parentsOfBadSubtrees.size} subtrees with their reparsed equivalents.`,
    parentsOfBadSubtrees,
  )
}
