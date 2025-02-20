import { type YRange } from '@/util/codemirror/yCollab/y-range'
import { ySyncAnnotation, type YSyncConfig, ySyncFacet } from '@/util/codemirror/yCollab/y-sync'
import { type StackItemEvent } from '@/util/codemirror/yCollab/yjsTypes'
import * as cmState from '@codemirror/state'
import * as cmView from '@codemirror/view'
import { createMutex } from 'lib0/mutex'
import * as Y from 'yjs'

/** TODO: Add docs */
export class YUndoManagerConfig {
  /** TODO: Add docs */
  constructor(readonly undoManager: Y.UndoManager) {}

  /** TODO: Add docs */
  addTrackedOrigin(origin: unknown) {
    this.undoManager.addTrackedOrigin(origin)
  }

  /** TODO: Add docs */
  removeTrackedOrigin(origin: unknown) {
    this.undoManager.removeTrackedOrigin(origin)
  }

  /**
   * @returns Whether a change was undone.
   */
  undo(): boolean {
    return this.undoManager.undo() != null
  }

  /**
   * @returns Whether a change was redone.
   */
  redo(): boolean {
    return this.undoManager.redo() != null
  }
}

export const yUndoManagerFacet = cmState.Facet.define<YUndoManagerConfig, YUndoManagerConfig>({
  combine(inputs) {
    return inputs[inputs.length - 1]!
  },
})

export const yUndoManagerAnnotation = cmState.Annotation.define<YUndoManagerConfig>()

class YUndoManagerPluginValue implements cmView.PluginValue {
  private readonly conf: YUndoManagerConfig
  private readonly syncConf: YSyncConfig
  private _beforeChangeSelection: null | YRange
  private readonly _undoManager: Y.UndoManager
  private readonly _mux: (cb: () => void, elseCb?: (() => void) | undefined) => any
  private readonly _storeSelection: () => void
  private readonly _onStackItemAdded: (event: StackItemEvent) => void
  private readonly _onStackItemPopped: (event: StackItemEvent) => void

  constructor(readonly view: cmView.EditorView) {
    this.conf = view.state.facet(yUndoManagerFacet)
    this._undoManager = this.conf.undoManager
    this.syncConf = view.state.facet(ySyncFacet)
    this._beforeChangeSelection = null
    this._mux = createMutex()

    this._onStackItemAdded = ({ stackItem, changedParentTypes }: StackItemEvent) => {
      // only store metadata if this type was affected
      if (
        changedParentTypes.has(this.syncConf.ytext as any) &&
        this._beforeChangeSelection &&
        !stackItem.meta.has(this)
      ) {
        // do not overwrite previous stored selection
        stackItem.meta.set(this, this._beforeChangeSelection)
      }
    }
    this._onStackItemPopped = ({ stackItem }: StackItemEvent) => {
      const sel = stackItem.meta.get(this)
      if (sel) {
        const selection = this.syncConf.fromYRange(sel)
        view.dispatch(
          view.state.update({
            selection,
            effects: [cmView.EditorView.scrollIntoView(selection)],
          }),
        )
        this._storeSelection()
      }
    }
    /**
     * Do this without mutex, simply use the sync annotation
     */
    this._storeSelection = () => {
      // store the selection before the change is applied so we can restore it with the undo manager.
      this._beforeChangeSelection = this.syncConf.toYRange(this.view.state.selection.main)
    }
    this._undoManager.on('stack-item-added', this._onStackItemAdded)
    this._undoManager.on('stack-item-popped', this._onStackItemPopped)
    this._undoManager.addTrackedOrigin(this.syncConf)
  }

  update(update: cmView.ViewUpdate) {
    if (
      update.selectionSet &&
      (update.transactions.length === 0 ||
        update.transactions[0]!.annotation(ySyncAnnotation) !== this.syncConf)
    ) {
      // This only works when YUndoManagerPlugin is included before the sync plugin
      this._storeSelection()
    }
  }

  destroy() {
    this._undoManager.off('stack-item-added', this._onStackItemAdded)
    this._undoManager.off('stack-item-popped', this._onStackItemPopped)
    this._undoManager.removeTrackedOrigin(this.syncConf)
  }
}
export const yUndoManager = cmView.ViewPlugin.fromClass(YUndoManagerPluginValue)

export const undo: cmState.StateCommand = ({ state }) =>
  state.facet(yUndoManagerFacet).undo() || true

export const redo: cmState.StateCommand = ({ state }) =>
  state.facet(yUndoManagerFacet).redo() || true

export const undoDepth = (state: cmState.EditorState): number =>
  state.facet(yUndoManagerFacet).undoManager.undoStack.length

export const redoDepth = (state: cmState.EditorState): number =>
  state.facet(yUndoManagerFacet).undoManager.redoStack.length

/**
 * Default key bindings for the undo manager.
 */
export const yUndoManagerKeymap: cmView.KeyBinding[] = [
  { key: 'Mod-z', run: undo, preventDefault: true },
  { key: 'Mod-y', mac: 'Mod-Shift-z', run: redo, preventDefault: true },
  { key: 'Mod-Shift-z', run: redo, preventDefault: true },
]
