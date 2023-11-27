import { reactive } from 'vue'
import * as Y from 'yjs'
import type { ExprId, Uuid } from './yjsModel'

declare const brandLineId: unique symbol
export type LineId = Uuid & { [brandLineId]: never }

export type DocLine = {
  id: LineId
  parent: LineId | undefined
  previous: LineId | undefined
  next: LineId | undefined
  text: Y.Text
}

export class ModuleDoc {
  lines: Y.Array<DocLine>

  constructor(ydoc: Y.Doc) {
    this.lines = ydoc.getArray('lines')
  }
}

const lines = reactive(new Map<LineId, DocLine>())

///////////////////

type Expression = {
  //fields: Y.Map<>

  children: ExprId[]
  metadata: any
  //type: ExpressionType
}

type Token = {
  value: Y.Text
}

type Block = {
  indentation: number
}

/*
Token edit:
  - Try reparsing with the node edited, to see if the syntax changed?
  - We don't want changes to affect parents... try to close everything that is opened...
 */

/*
Invalid:
  - sometimes we have some knowledge of substructure; it would be nice if we could use it

 */
