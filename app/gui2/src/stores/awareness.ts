import { Vec2 } from '@/util/data/vec2'
import { reactive } from 'vue'
import { Awareness as YjsAwareness } from 'y-protocols/awareness'
import type { StackItem } from 'ydoc-shared/languageServerTypes'
import * as Y from 'yjs'

// === Public types ===

export type FileName = string

export interface UploadingFile {
  sizePercentage: number
  stackItem: StackItem
  position?: Vec2
  portId?: string
}

// === Awareness wrapper ===

/**
 * A thin wrapper around `Awareness` from `yjs`, providing helper methods for Enso IDE-specific state.
 */
export class Awareness {
  public internal: YjsAwareness

  constructor(doc: Y.Doc) {
    this.internal = new YjsAwareness(doc)
    this.internal.setLocalState(initialState())
    this.internal.states = reactive(this.internal.states)
  }

  public addOrUpdateUpload(name: FileName, file: UploadingFile) {
    this.mutateLocalState((state) => {
      state.uploads[name] = { ...(state.uploads[name] ?? {}), ...file }
    })
  }

  public removeUpload(name: FileName) {
    this.mutateLocalState((state) => {
      delete state.uploads[name]
    })
  }

  public allUploads(): [FileName, UploadingFile][] {
    return [...this.states.values()].flatMap((state) => [...Object.entries(state.uploads)])
  }

  private get states(): Map<number, State> {
    return this.internal.states as Map<number, State>
  }

  private mutateLocalState(f: (state: State) => void) {
    const state = this.internal.getLocalState() as State
    f(state)
    this.internal.setLocalState(state)
  }
}

// === Private types ===

interface State {
  uploads: Record<FileName, UploadingFile>
}

function initialState(): State {
  return {
    uploads: {},
  }
}
