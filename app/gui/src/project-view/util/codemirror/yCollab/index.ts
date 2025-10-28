/**
 * @file CodeMirror extension for synchronizing with a Yjs Text object.
 * Based on <https://github.com/yjs/y-codemirror.next>. Initial changes from upstream:
 * - Translated from JSDoc-typed JS to Typescript.
 * - Refactored for stricter typing.
 * - Changes to match project code style.
 */

import type { Extension } from '@codemirror/state'
import type { Awareness } from 'y-protocols/awareness.js'
import type { LocalUserActionOrigin } from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'
import { YRange } from './y-range'
import { yRemoteSelections, yRemoteSelectionsTheme } from './y-remote-selections'
import { YSyncConfig, ySync, ySyncAnnotation, ySyncFacet } from './y-sync'
import { yUndoManagerKeymap } from './y-undomanager'
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

/** CodeMirror Extension for synchronizing the editor state with a {@link Y.Text}. */
export function yCollab(
  ytext: Y.Text & { doc: Y.Doc },
  awareness: Awareness | null,
  origin?: LocalUserActionOrigin | undefined,
): Extension {
  const ySyncConfig = new YSyncConfig(ytext, awareness, origin)
  const plugins = [ySyncFacet.of(ySyncConfig), ySync]
  if (awareness) {
    plugins.push(yRemoteSelectionsTheme, yRemoteSelections)
  }
  return plugins
}
