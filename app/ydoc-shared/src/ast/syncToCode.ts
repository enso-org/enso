import * as iter from 'enso-common/src/utilities/data/iter'
import * as map from 'lib0/map'
import { assert, assertDefined } from '../util/assert'
import {
  type SourceRange,
  type SourceRangeEdit,
  type SourceRangeEditDesc,
  type SourceRangeKey,
  type SpanTree,
  applyTextEdits,
  applyTextEditsToSpans,
  enclosingSpans,
  rangeLength,
  sourceRangeFromKey,
  sourceRangeKey,
  textChangeToEdits,
  trimEnd,
} from '../util/data/text'
import { xxHash128 } from './ffi'
import { type NodeKey, type NodeSpanMap, newExternalId } from './idMap'
import type { Module, MutableModule } from './mutableModule'
import { parseInSameContext } from './parse'
import { printWithSpans } from './print'
import { isTokenId } from './token'
import {
  Assignment,
  Ast,
  type AstId,
  MutableAssignment,
  type MutableAst,
  rewriteRefs,
  syncFields,
  syncNodeMetadata,
} from './tree'

/**
 * Recursion helper for {@link syntaxHash}.
 */
function hashSubtreeSyntax(ast: Ast, hashesOut: Map<SyntaxHash, Ast[]>): SyntaxHash {
  let content = ''
  content += ast.typeName + ':'
  for (const child of ast.concreteChildren({ verbatim: false, indent: '' })) {
    content += child.whitespace ?? '?'
    if (isTokenId(child.node)) {
      content += 'Token:' + hashString(ast.module.getToken(child.node).code())
    } else {
      content += hashSubtreeSyntax(ast.module.get(child.node), hashesOut)
    }
  }
  const astHash = hashString(content)
  map.setIfUndefined(hashesOut, astHash, (): Ast[] => []).unshift(ast)
  return astHash
}

declare const brandHash: unique symbol
/** See {@link syntaxHash}. */
type SyntaxHash = string & { [brandHash]: never }

/** Applies the syntax-data hashing function to the input, and brands the result as a `SyntaxHash`. */
function hashString(input: string): SyntaxHash {
  return xxHash128(input) as SyntaxHash
}

/**
 * Calculates `SyntaxHash`es for the given node and all its children.
 *
 *  Each `SyntaxHash` summarizes the syntactic content of an AST. If two ASTs have the same code and were parsed the
 *  same way (i.e. one was not parsed in a context that resulted in a different interpretation), they will have the same
 *  hash. Note that the hash is invariant to metadata, including `externalId` assignments.
 */
function syntaxHash(root: Ast) {
  const hashes = new Map<SyntaxHash, Ast[]>()
  const rootHash = hashSubtreeSyntax(root, hashes)
  return { root: rootHash, hashes }
}

/** Update `ast` to match the given source code, while modifying it as little as possible. */
export function syncToCode(ast: MutableAst, code: string, metadataSource?: Module) {
  const codeBefore = ast.code()
  const textEdits = textChangeToEdits(codeBefore, code)
  applyTextEditsToAst(ast, textEdits, metadataSource ?? ast.module)
}

/** Find nodes in the input `ast` that should be treated as equivalents of nodes in `parsedRoot`. */
function calculateCorrespondence(
  ast: Ast,
  astSpans: NodeSpanMap,
  parsedRoot: Ast,
  parsedSpans: NodeSpanMap,
  textEdits: SourceRangeEditDesc[],
  codeAfter: string,
): Map<AstId, Ast> {
  const newSpans = new Map<AstId, SourceRange>()
  for (const [key, asts] of parsedSpans) {
    for (const ast of asts) newSpans.set(ast.id, sourceRangeFromKey(key))
  }

  // Retained-code matching: For each new tree, check for some old tree of the same type such that the new tree is the
  // smallest node to contain all characters of the old tree's code that were not deleted in the edit.
  //
  // If the new node's span exactly matches the retained code, add the match to `toSync`. If the new node's span
  // contains additional code, add the match to `candidates`.
  const toSync = new Map<AstId, Ast>()
  const candidates = new Map<AstId, Ast>()
  const allSpansBefore = Array.from(astSpans.keys(), sourceRangeFromKey)
  const spansBeforeAndAfter = applyTextEditsToSpans(textEdits, allSpansBefore).map(
    ([before, after]) => [before, trimEnd(after, codeAfter)] satisfies [any, any],
  )
  const partAfterToAstBefore = new Map<SourceRangeKey, Ast>()
  for (const [spanBefore, partAfter] of spansBeforeAndAfter) {
    const astBefore = astSpans.get(sourceRangeKey(spanBefore) as NodeKey)![0]!
    partAfterToAstBefore.set(sourceRangeKey(partAfter), astBefore)
  }
  const matchingPartsAfter = spansBeforeAndAfter.map(([_before, after]) => after)
  const parsedSpanTree = new AstWithSpans(parsedRoot, (id) => newSpans.get(id)!)
  const astsMatchingPartsAfter = enclosingSpans(parsedSpanTree, matchingPartsAfter)
  for (const [astAfter, partsAfter] of astsMatchingPartsAfter) {
    for (const partAfter of partsAfter) {
      const astBefore = partAfterToAstBefore.get(sourceRangeKey(partAfter))!
      if (astBefore.typeName === astAfter.typeName) {
        ;(rangeLength(newSpans.get(astAfter.id)!) === rangeLength(partAfter) ?
          toSync
        : candidates
        ).set(astBefore.id, astAfter)
        break
      }
    }
  }

  // Index the matched nodes.
  const oldIdsMatched = new Set<AstId>()
  const newIdsMatched = new Set<AstId>()
  for (const [oldId, newAst] of toSync) {
    oldIdsMatched.add(oldId)
    newIdsMatched.add(newAst.id)
  }

  // Movement matching: For each new tree that hasn't been matched, match it with any identical unmatched old tree.
  const newHashes = syntaxHash(parsedRoot).hashes
  const oldHashes = syntaxHash(ast).hashes
  for (const [hash, newAsts] of newHashes) {
    const unmatchedNewAsts = newAsts.filter((ast) => !newIdsMatched.has(ast.id))
    const unmatchedOldAsts = oldHashes.get(hash)?.filter((ast) => !oldIdsMatched.has(ast.id)) ?? []
    for (const [unmatchedNew, unmatchedOld] of iter.zip(unmatchedNewAsts, unmatchedOldAsts)) {
      if (unmatchedNew.typeName === unmatchedOld.typeName) {
        toSync.set(unmatchedOld.id, unmatchedNew)
        // Update the matched-IDs indices.
        oldIdsMatched.add(unmatchedOld.id)
        newIdsMatched.add(unmatchedNew.id)
      }
    }
  }

  // Apply any non-optimal span matches from `candidates`, if the nodes involved were not matched during
  // movement-matching.
  for (const [beforeId, after] of candidates) {
    if (oldIdsMatched.has(beforeId) || newIdsMatched.has(after.id)) continue
    if (after.typeName === ast.module.get(beforeId).typeName) {
      toSync.set(beforeId, after)
    }
  }

  for (const [idBefore, astAfter] of toSync.entries())
    assert(ast.module.get(idBefore).typeName === astAfter.typeName)
  return toSync
}

/** Update `ast` according to changes to its corresponding source code. */
export function applyTextEditsToAst(
  ast: MutableAst,
  textEdits: SourceRangeEdit[],
  metadataSource: Module,
) {
  const printed = printWithSpans(ast)
  const code = applyTextEdits(printed.code, textEdits)
  const parsed = parseInSameContext(code, ast)
  const toSync = calculateCorrespondence(
    ast,
    printed.info.nodes,
    parsed.root,
    parsed.spans.nodes,
    textEdits,
    code,
  )
  ast.module.transact(() => syncTree(ast, parsed.root, toSync, ast.module, metadataSource))
}

/** Replace `target` with `newContent`, reusing nodes according to the correspondence in `toSync`. */
function syncTree(
  target: MutableAst,
  newContent: Ast,
  toSync: Map<AstId, Ast>,
  edit: MutableModule,
  metadataSource: Module,
) {
  const newIdToEquivalent = new Map<AstId, AstId>()
  for (const [beforeId, after] of toSync) newIdToEquivalent.set(after.id, beforeId)
  const childSplicerFor = (parentId: AstId) => (id: AstId) => {
    const original = newIdToEquivalent.get(id)
    if (original) {
      const replacement = edit.get(original)
      if (replacement.parentId !== parentId) replacement.fields.set('parent', parentId)
      return original
    } else {
      const child = edit.splice(newContent.module.get(id)!)
      if (child.parentId !== parentId) child.fields.set('parent', parentId)
    }
  }
  const parentId = target.fields.get('parent')
  assertDefined(parentId)
  const parent = edit.get(parentId)
  const targetSyncEquivalent = toSync.get(target.id)
  const syncRoot = targetSyncEquivalent?.id === newContent.id ? targetSyncEquivalent : undefined
  let newRoot: MutableAst
  if (syncRoot) {
    newRoot = target
  } else {
    const spliced = edit.splice(newContent)
    parent.replaceChild(target.id, spliced)
    newRoot = spliced
    newRoot.fields.set('metadata', target.fields.get('metadata').clone())
    target.fields.get('metadata').set('externalId', newExternalId())
  }
  newRoot.visitRecursive((ast) => {
    const syncFieldsFrom = toSync.get(ast.id)
    const editAst = edit.getVersion(ast)
    if (syncFieldsFrom) {
      const originalAssignmentExpression =
        ast instanceof Assignment ?
          metadataSource.get(ast.fields.get('expression').node)
        : undefined
      syncFields(editAst, syncFieldsFrom, childSplicerFor(ast.id))
      if (editAst instanceof MutableAssignment && originalAssignmentExpression) {
        if (editAst.expression.externalId !== originalAssignmentExpression.externalId)
          editAst.expression.setExternalId(originalAssignmentExpression.externalId)
        syncNodeMetadata(
          editAst.expression.mutableNodeMetadata(),
          originalAssignmentExpression.nodeMetadata,
        )
      }
    } else {
      rewriteRefs(editAst, childSplicerFor(ast.id))
    }
    return true
  })
}

/** Provides a `SpanTree` view of an `Ast`, given span information. */
class AstWithSpans implements SpanTree<Ast> {
  private readonly ast: Ast
  private readonly getSpan: (astId: AstId) => SourceRange

  constructor(ast: Ast, getSpan: (astId: AstId) => SourceRange) {
    this.ast = ast
    this.getSpan = getSpan
  }

  id(): Ast {
    return this.ast
  }

  span(): SourceRange {
    return this.getSpan(this.ast.id)
  }

  *children(): IterableIterator<SpanTree<Ast>> {
    for (const child of this.ast.children()) {
      if (child instanceof Ast) yield new AstWithSpans(child, this.getSpan)
    }
  }
}
