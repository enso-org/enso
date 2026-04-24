/**
 * @file This module supports recognizing a lezer parse tree, to build a higher-level,
 * strongly typed syntax tree better suited for more complex analyses.
 */
import type { TreeCursor } from '@lezer/common'

export interface Range {
  from: number
  to: number
}

export type SyntaxNode = FunctionNode | ColumnNode | BinOpNode | PrefixOpNode

export interface FunctionNode {
  type: 'Function'
  name: Range
  open: Range | undefined
  args: Range | undefined
  close: Range | undefined
}

export interface ColumnNode {
  type: 'Column'
  open: Range
  name: Range
  close: Range | undefined
}

export interface BinOpNode<T = Range> {
  type: 'BinOp'
  lhs: T
  op: Range
  rhs: T
}

export interface PrefixOpNode<T = Range> {
  type: 'PrefixOp'
  op: Range
  rhs: T
}

export function parseNode(cursor: TreeCursor): SyntaxNode | null {
  const { child, childOpt, siblingOpt, match, anyChild, anySibling } = useParserCombinators(cursor)
  return match<SyntaxNode | null>({
    Function: (func) =>
      childOpt('OpenParen', (open) =>
        siblingOpt('CloseParen', (close) => ({
          type: 'Function',
          name: open ? { from: func.from, to: open.from } : func,
          open,
          args: optRange(func.from, close?.from ?? func.to),
          close,
        })),
      ),
    Column: (column) =>
      child('OpenBracket', (open) =>
        siblingOpt('CloseBracket', (close) => ({
          type: 'Column',
          open,
          name: { from: open.to, to: close?.from ?? column.to },
          close,
        })),
      ),
    BinOpApp: () =>
      anyChild((lhs) =>
        anySibling((op) =>
          anySibling((rhs) => ({
            type: 'BinOp',
            lhs,
            op,
            rhs,
          })),
        ),
      ),
    PrefixOpApp: (opApp) =>
      anyChild((op) => ({
        type: 'PrefixOp',
        op,
        rhs: { from: op.to, to: opApp.to },
      })),
  })
}

function optRange(from: number, to: number): Range | undefined {
  return from < to ? { from, to } : undefined
}

function useParserCombinators(cursor: TreeCursor) {
  function pos(cursor: TreeCursor): Range {
    return { from: cursor.from, to: cursor.to }
  }
  function childOpt<T>(name: string, then: (match: Range | undefined) => T): T {
    return then(cursor.firstChild() && cursor.name === name ? pos(cursor) : undefined)
  }
  function siblingOpt<T>(name: string, then: (match: Range | undefined) => T): T {
    return then(cursor.nextSibling() && cursor.name === name ? pos(cursor) : undefined)
  }
  function child<T>(name: string, then: (match: Range) => T): T | null {
    return childOpt(name, (match) => (match ? then(match) : null))
  }
  function sibling<T>(name: string, then: (match: Range) => T): T | null {
    return siblingOpt(name, (match) => (match ? then(match) : null))
  }
  function match<T>(then: Record<string, (match: Range) => T>): T | null {
    return then[cursor.name]?.(pos(cursor)) ?? null
  }
  function anyChild<T>(then: (match: Range) => T): T | null {
    return cursor.firstChild() ? then(pos(cursor)) : null
  }
  function anySibling<T>(then: (match: Range) => T): T | null {
    return cursor.nextSibling() ? then(pos(cursor)) : null
  }
  return { childOpt, siblingOpt, child, sibling, match, anyChild, anySibling }
}
