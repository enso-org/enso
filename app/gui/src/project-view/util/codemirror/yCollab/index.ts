/**
 * @file CodeMirror extension for synchronizing with a Yjs Text object.
 * Based on <https://github.com/yjs/y-codemirror.next>. Initial changes from upstream:
 * - Translated from JSDoc-typed JS to Typescript.
 * - Refactored for stricter typing.
 * - Changes to match project code style.
 */

import * as cmView from '@codemirror/view'
import { type Awareness } from 'y-protocols/awareness.js'
import * as Y from 'yjs'
import { YRange } from './y-range'
import { yRemoteSelections, yRemoteSelectionsTheme } from './y-remote-selections'
import { YSyncConfig, ySync, ySyncAnnotation, ySyncFacet } from './y-sync'
import {
  YUndoManagerConfig,
  redo,
  undo,
  yUndoManager,
  yUndoManagerFacet,
  yUndoManagerKeymap,
} from './y-undomanager'
export {
  YRange,
  YSyncConfig,
  yRemoteSelections,
  yRemoteSelectionsTheme,
  ySync,
  ySyncAnnotation,
  ySyncFacet,
  yUndoManagerKeymap,
}

/* CodeMirror Extension for synchronizing the editor state with a {@link Y.Text}. */
export const yCollab = (
  ytext: Y.Text & { doc: Y.Doc },
  awareness: Awareness | null,
  {
    undoManager = new Y.UndoManager(ytext),
  }: {
    /** Set to false to disable the undo-redo plugin */
    undoManager?: Y.UndoManager | false
  } = {},
) => {
  const ySyncConfig = new YSyncConfig(ytext, awareness)
  const plugins = [ySyncFacet.of(ySyncConfig), ySync]
  if (awareness) {
    plugins.push(yRemoteSelectionsTheme, yRemoteSelections)
  }
  if (undoManager !== false) {
    // By default, only track changes that are produced by the sync plugin (local edits)
    plugins.push(
      yUndoManagerFacet.of(new YUndoManagerConfig(undoManager)),
      yUndoManager,
      cmView.EditorView.domEventHandlers({
        beforeinput(e, view) {
          if (e.inputType === 'historyUndo') return undo(view)
          if (e.inputType === 'historyRedo') return redo(view)
          return false
        },
      }),
    )
  }
  return plugins
}
