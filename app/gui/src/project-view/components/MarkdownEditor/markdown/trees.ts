/** @file Lezer Tree operations for reading and writing inline formatting state. */
import {
  zeroFormatDepths,
  type FormatDepths,
  type FormatNode,
  type NormalizedRange,
  type SeminormalizedRange,
  type TrimmedRange,
} from '@/components/MarkdownEditor/markdown/types'
import { type SyntaxNodeRef, type Tree, type TreeCursor } from '@lezer/common'
import { identity } from '@vueuse/core'
import { Range } from 'ydoc-shared/util/data/range'

function reversed<T>(elements: ReadonlyArray<T>): T[] {
  return elements.slice().reverse()
}

interface Side {
  fromOrTo: 'from' | 'to'
  toOrFrom: 'to' | 'from'
  inside: 1 | -1
  outside: -1 | 1
  innermost: (a: number, b: number) => number
  outermost: (a: number, b: number) => number
  insideOrder: <T>(nodes: T[]) => T[]
  outsideOrder: <T>(nodes: T[]) => T[]
}
const FROM: Side = {
  fromOrTo: 'from',
  toOrFrom: 'to',
  inside: 1,
  outside: -1,
  innermost: Math.max,
  outermost: Math.min,
  insideOrder: identity,
  outsideOrder: reversed,
}
const TO: Side = {
  fromOrTo: 'to',
  toOrFrom: 'from',
  inside: -1,
  outside: 1,
  innermost: Math.min,
  outermost: Math.max,
  insideOrder: reversed,
  outsideOrder: identity,
}

function sides<T>(f: (side: Side) => T): { from: T; to: T } {
  return {
    from: f(FROM),
    to: f(TO),
  }
}

const MARK_NODE: Readonly<Record<FormatNode, string>> = {
  Emphasis: 'EmphasisMark',
  StrongEmphasis: 'EmphasisMark',
  Strikethrough: 'StrikethroughMark',
}

abstract class TreeRangeVisitor {
  protected constructor(protected readonly range: Range) {}

  protected abstract enter(node: SyntaxNodeRef): boolean | void
  protected leave(_node: SyntaxNodeRef): boolean | void {}

  abstract visit(tree: Tree): void
}

abstract class ExclusiveTreeVisitor extends TreeRangeVisitor {
  visit(tree: Tree) {
    tree.iterate({
      from: this.range.from,
      to: this.range.to,
      enter: (node) => {
        // `iterate` is inclusive of nodes that just-meet the specified range
        if (node.to === this.range.from || node.from === this.range.to) return false
        return this.enter(node)
      },
      leave: this.leave.bind(this),
    })
  }
}

abstract class ContainedNodeVisitor extends TreeRangeVisitor {
  visit(tree: Tree) {
    tree.iterate({
      from: this.range.from,
      to: this.range.to,
      enter: (node) => {
        // `iterate` is inclusive of nodes that just-meet the specified range (see {@link ExclusiveTreeVisitor}).
        if (node.to === this.range.from || node.from === this.range.to) return false
        if (this.range.contains(nodeRange(node))) return this.enter(node)
      },
      leave: (node) => {
        if (this.range.contains(nodeRange(node))) this.leave(node)
      },
    })
  }
}

/**
 * @returns The depths of formatting nodes the point is within, or `undefined` if the point is within an unformattable
 * block.
 */
export function pointFormatAncestorInfo(
  pos: number,
  tree: Tree,
):
  | {
      formatDepth: Readonly<FormatDepths>
      /** True if the position is within an inline node where formatting delimiters are not recognized. */
      unformattable: boolean
    }
  | undefined {
  const cursor = tree.cursorAt(pos, 0)
  const formatDepth = zeroFormatDepths()
  let unformattable = false
  do {
    switch (cursor.name) {
      case 'Emphasis':
      case 'StrongEmphasis':
      case 'Strikethrough':
        formatDepth[cursor.name] += 1
        break
      case 'InlineCode':
      case 'Autolink':
        unformattable = true
        break
      case 'Link':
        // TODO: AllowFormattingLinkText
        unformattable = true
        break
      case 'FencedCode':
        return undefined
    }
  } while (cursor.parent())
  return { formatDepth, unformattable }
}

/** @returns The containing unformattable inline node. The caller should first determine that such a node is present. */
export function getUnformattableAncestor(pos: number, tree: Tree): Range {
  const cursor = tree.cursorAt(pos, 0)
  LOOP: do {
    switch (cursor.name) {
      case 'InlineCode':
      case 'Autolink':
        break LOOP
      case 'Link':
        // TODO: AllowFormattingLinkText
        break LOOP
    }
  } while (cursor.parent())
  return nodeRange(cursor)
}

class AnalyzeContainedDelimiters extends ContainedNodeVisitor {
  private ancestorIsNodeType: number = 0

  constructor(
    range: Range,
    private readonly nodeType: FormatNode,
    private readonly emit: (range: Range) => void,
  ) {
    super(range)
  }

  enter(node: SyntaxNodeRef) {
    if (this.ancestorIsNodeType & 1 && node.name === MARK_NODE[this.nodeType])
      this.emit(nodeRange(node))
    this.ancestorIsNodeType = (this.ancestorIsNodeType << 1) | +(node.name === this.nodeType)
  }
  override leave() {
    this.ancestorIsNodeType >>= 1
  }
}

/** For each node of the specified type fully-contained in the given range, apply the visitor to its delimiters. */
export function visitContainedDelimiters(
  range: Range,
  tree: Tree,
  nodeType: FormatNode,
  visit: (range: Range) => void,
) {
  new AnalyzeContainedDelimiters(range, nodeType, visit).visit(tree)
}

/**
 * Extract the current range from the given node.
 */
function nodeRange(node: Readonly<SyntaxNodeRef>): Range {
  return Range.tryFromBounds(node.from, node.to)!
}

/** Returns whether the node is formatting markup. */
export function isDelimiter(nodeName: string) {
  switch (nodeName) {
    case 'EmphasisMark':
    case 'StrikethroughMark':
    case 'CodeMark':
    case 'LinkMark':
      return true
    default:
      return false
  }
}

/** Contract the range to exclude any delimiters that are oriented the wrong way. */
export function trimRangeDelimiters(range: Range, tree: Tree): TrimmedRange {
  const cursor = tree.cursor()
  return Range.tryFromBounds(
    trimDelimiter.from(range.from, cursor),
    trimDelimiter.to(range.to, cursor),
  )! as TrimmedRange
}
const trimDelimiter = sides(({ toOrFrom, inside }) => (pos: number, cursor: TreeCursor) => {
  while (cursor.moveTo(pos, inside) && isDelimiter(cursor.name)) {
    const insideMark = cursor[toOrFrom]
    cursor.parent()
    if (cursor[toOrFrom] !== insideMark) break
    pos = insideMark
  }
  return pos
})

/**
 * Split the given range into parts that can have inline formatting applied to them, and yield them to the provided
 * visitors. Note that ranges will not necessarily be yielded in document order.
 */
export function splitRange(
  range: TrimmedRange,
  tree: Tree,
  visit: (range: NormalizedRange) => void,
  trimRange: (range: Range) => Range,
) {
  if (range.from >= range.to) return
  const normalized = normalizeRange(range, tree)
  if (!normalized) return
  new RangeSplitter(normalized, tree, visit, trimRange).visit(tree)
}

class RangeSplitter extends ExclusiveTreeVisitor {
  /** Depth of currently-entered node in AST: 0 is the `Document`; 1 is a block-level node; deeper nodes are inline. */
  private depth: number = 0
  /**
   * When a block node that is partially-covered by `range` has been entered, this is initially set to the intersection
   * of the node, refined by visiting any children, and emitted when leaving the block node.
   */
  private currentRange: Range | undefined = undefined

  constructor(
    range: TrimmedRange,
    private readonly tree: Tree,
    private readonly emit: (range: NormalizedRange) => void,
    private readonly trimRange: (range: Range) => Range,
  ) {
    super(range)
  }

  enter(node: SyntaxNodeRef): boolean {
    const nodeFromOutside = node.from < this.range.from
    const nodeToOutside = this.range.to < node.to
    if (this.depth === 1) {
      if (!this.enterBlock(node, !nodeFromOutside && !nodeToOutside)) return false
    } else if (this.depth && nodeFromOutside !== nodeToOutside) {
      if (!this.enterPartialInline(node, nodeFromOutside)) return false
    }
    this.depth += 1
    return true
  }

  private enterBlock(node: SyntaxNodeRef, nodeFullyInRange: boolean): boolean {
    // TODO: Exclude block delimiter
    const blockRange = nodeRange(node)
    if (nodeFullyInRange) {
      if (blockRange.to !== blockRange.from) {
        switch (node.name) {
          case 'CodeBlock':
          case 'FencedCode':
            break
          default:
            this.emit(blockRange as NormalizedRange)
        }
      }
      return false
    }
    this.currentRange = this.range.tryIntersect(blockRange)!
    return true
  }

  private enterPartialInline(node: SyntaxNodeRef, nodeFromOutside: boolean): boolean {
    switch (node.name) {
      case 'Link':
      // TODO: Yield the intersection of the range with the link text.
      /* fallthrough */
      case 'Autolink':
      case 'InlineCode': {
        // Exclude the node from the range.
        this.currentRange = this.trimRange(
          nodeFromOutside ?
            Range.tryFromBounds(node.to, this.currentRange!.to)!
          : Range.tryFromBounds(this.currentRange!.from, node.from)!,
        )
        return false
      }
      // FIXME: Maybe the default should be unformattable?
      default:
        return true
    }
  }

  override leave() {
    this.depth -= 1
    // Yield the main matched content of a partially-included block element.
    if (this.depth === 1) {
      const normalized = normalizeRange(this.currentRange as SeminormalizedRange, this.tree)
      if (normalized) this.emit(normalized)
    }
  }
}

export function normalizeRange(
  { from, to }: SeminormalizedRange,
  tree: Tree,
): NormalizedRange | undefined
// This delimiter operation can be applied to any trimmed range (i.e. before range splitting); in that case the result
// won't be a fully normalized range.
export function normalizeRange({ from, to }: TrimmedRange, tree: Tree): TrimmedRange | undefined
/**
 * Adjust the ends of the range to include/exclude delimiters based on tree structure (see {@link NormalizedRange});
 * returns `undefined` if the resulting range contains no formattable content.
 */
export function normalizeRange(range: TrimmedRange, tree: Tree): TrimmedRange | undefined {
  const cursor = tree.cursor()

  const expandedRange = Range.tryFromBounds(
    includeWrappingDelimiters.from(range.from, cursor),
    includeWrappingDelimiters.to(range.to, cursor),
  )!

  if (insideInlineUnformattable(expandedRange, cursor)) return

  // For any allowed input, the result will be a {@link TrimmedRange}; if the input is a {@link SeminormalizedRange},
  // the result will be a {@link NormalizedRange}.
  return Range.tryFromBounds(
    excludeExcessDelimiters.from(expandedRange, cursor),
    excludeExcessDelimiters.to(expandedRange, cursor),
  ) as TrimmedRange | undefined
}
/** Expand the range to include delimiters of nodes that it contains part of. */
const includeWrappingDelimiters = sides(
  ({ fromOrTo, outside }) =>
    (pos: number, cursor: TreeCursor) => {
      while (cursor.moveTo(pos, outside) && isDelimiter(cursor.name)) {
        const expanded = cursor[fromOrTo]
        cursor.parent()
        if (expanded !== cursor[fromOrTo] || expanded === pos) break
        pos = expanded
      }
      return pos
    },
)
/** Contract the range to exclude delimiters of nodes that it doesn't contain all of. */
const excludeExcessDelimiters = sides(
  ({ fromOrTo, toOrFrom, inside }) =>
    (range: Range, cursor: TreeCursor) => {
      let pos = range[fromOrTo]
      while (cursor.moveTo(pos, inside) && isDelimiter(cursor.name)) {
        const contracted = cursor[toOrFrom]
        cursor.parent()
        const currentRange =
          fromOrTo === 'from' ?
            Range.tryFromBounds(pos, range.to)!
          : Range.tryFromBounds(range.from, pos)!
        if (currentRange.contains(nodeRange(cursor))) break
        pos = contracted
      }
      return pos
    },
)
function insideInlineUnformattable(range: Range, cursor: TreeCursor) {
  cursor.moveTo(range.from, 0)
  while (!nodeRange(cursor).encloses(range)) if (!cursor.parent()) return false
  do {
    switch (cursor.name) {
      case 'Link': // TODO: AllowFormattingLinkText
      case 'Autolink':
      case 'InlineCode':
      case 'URL':
        return true
    }
  } while (cursor.parent())
  return false
}

/** Expand each end of the range to the outermost node that it includes that end of the non-delimiter content of. */
export function denormalizeRange(range: NormalizedRange, tree: Tree): Range {
  const cursor = tree.cursor()
  return Range.tryFromBounds(
    includeWrappingDelimiters.from(range.from, cursor),
    includeWrappingDelimiters.to(range.to, cursor),
  )!
}

/**
 * If nodes of the given type are closed just before or opened just after the provided (expanded) range, returns them.
 */
export function analyzeMerges(
  tree: Tree,
  range: Range,
  nodeType: FormatNode,
): {
  from: Range | undefined
  to: Range | undefined
} {
  const cursor = tree.cursor()
  return {
    from: merge.from(range.from, cursor, nodeType),
    to: merge.to(range.to, cursor, nodeType),
  }
}
const merge = sides(({ outside }) => (pos: number, cursor: TreeCursor, nodeType: FormatNode) => {
  cursor.moveTo(pos, outside)
  if (!isDelimiter(cursor.name)) return
  const mark = nodeRange(cursor)
  cursor.parent()
  return cursor.name === nodeType ? mark : undefined
})

/** Returns the nodes that are opened outside the range and closed inside it. */
export function analyzeSplits(
  tree: Tree,
  range: NormalizedRange,
): {
  from: { name: string; delimiter: Range }[]
  to: { name: string; delimiter: Range }[]
} {
  const visitor = new AnalyzeSplits(range)
  visitor.visit(tree)
  return visitor.partlyOutside
}
class AnalyzeSplits extends ExclusiveTreeVisitor {
  readonly partlyOutside: {
    from: { name: string; delimiter: Range }[]
    to: { name: string; delimiter: Range }[]
  } = { from: [], to: [] }

  constructor(range: NormalizedRange) {
    super(range)
  }

  enter(node: SyntaxNodeRef) {
    const fromOutside = node.from < this.range.from
    const toOutside = this.range.to < node.to
    if (fromOutside !== toOutside) {
      switch (node.name) {
        case 'Document':
        case 'Paragraph':
          break
        case 'Emphasis':
        case 'StrongEmphasis':
        case 'Strikethrough':
          if (fromOutside) {
            this.partlyOutside.from.push({
              name: node.name,
              delimiter: nodeRange(node.node.lastChild!),
            })
          } else {
            this.partlyOutside.to.push({
              name: node.name,
              delimiter: nodeRange(node.node.firstChild!),
            })
          }
          break
        default:
          console.warn(`analyzeRangeFormatting: Unexpected node type: ${node.name}.`)
      }
    }
  }
}

/**
 * Analyze the current formatting of the given range as relates to removing the specified format.
 */
export function analyzeRangeFormattingRemoval(
  tree: Tree,
  range: NormalizedRange,
  nodeType: FormatNode,
): {
  fromOutside: { name: string; delimiter: Range }[]
  toOutside: { name: string; delimiter: Range }[]
  outerNodesToClose: { name: string; delimiter: Range }[]
  remove: Range[]
} {
  const extended = denormalizeRange(range, tree)
  const visitor = new AnalyzeRemoval(range, extended, nodeType)
  visitor.visit(tree)
  const { partlyOutside, outerNodesToClose, contractAtFrom, contractAtTo } = visitor
  const remove = [...contractAtFrom, ...contractAtTo]
    .filter(({ name }) => name === nodeType)
    .map(({ delimiter }) => delimiter)
  return {
    fromOutside: [...partlyOutside.from, ...contractAtTo],
    toOutside: [...partlyOutside.to, ...contractAtFrom],
    outerNodesToClose,
    remove,
  }
}
class AnalyzeRemoval extends AnalyzeSplits {
  readonly outerNodesToClose: { name: string; delimiter: Range }[] = []
  readonly contractAtFrom: { name: string; delimiter: Range }[] = []
  readonly contractAtTo: { name: string; delimiter: Range }[] = []
  private insideNodeType = false

  constructor(
    range: NormalizedRange,
    private readonly extended: Range,
    private readonly nodeType: string,
  ) {
    super(range)
  }

  override enter(node: SyntaxNodeRef) {
    super.enter(node)
    if (node.from < this.range.from && this.range.to < node.to) {
      if (node.name === this.nodeType) this.insideNodeType = true
      if (this.insideNodeType) {
        if (this.extended.from <= node.from) {
          this.contractAtFrom.push({
            name: node.name,
            delimiter: nodeRange(node.node.firstChild!),
          })
        } else if (node.to <= this.extended.to) {
          this.contractAtTo.push({
            name: node.name,
            delimiter: nodeRange(node.node.lastChild!),
          })
        } else {
          this.outerNodesToClose.push({
            name: node.name,
            delimiter: nodeRange(node.node.firstChild!),
          })
        }
      }
    }
  }
}

export const splitNodesAt = sides(
  ({ insideOrder, outsideOrder }) =>
    <T extends { readonly name: string }>(
      nodes: T[],
      nodeType: string,
    ): { inside: T[]; outside: T[] } => ({
      inside: insideOrder(nodes.filter(({ name }) => name !== nodeType)),
      outside: outsideOrder(nodes),
    }),
)
