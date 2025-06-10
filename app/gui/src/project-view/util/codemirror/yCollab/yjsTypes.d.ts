/** @file Types exposed by Yjs APIs, but not exported by name. */

import * as Y from 'yjs'

export interface StackItemEvent {
  stackItem: StackItem
  origin: unknown
  type: 'undo' | 'redo'
  changedParentTypes: Map<Y.AbstractType<Y.YEvent<any>>, Y.YEvent<any>[]>
}

export interface StackItem {
  insertions: DeleteSet
  deletions: DeleteSet
  /**
   * Use this to save and restore metadata like selection range
   */
  meta: Map<any, any>
}

export interface DeleteSet {
  clients: Map<number, DeleteItem[]>
}

export interface DeleteItem {
  clock: number
  len: number
}
