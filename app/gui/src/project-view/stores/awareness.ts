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
  position: Vec2
}

// === Awareness wrapper ===

/** A thin wrapper around `Awareness` from `yjs`, providing helper methods for Enso IDE-specific state. */
export class Awareness {
  public internal: YjsAwareness
  private uploadingFiles: Map<ClientId, Uploads>

  /** TODO: Add docs */
  constructor(doc: Y.Doc) {
    this.internal = new YjsAwareness(doc)
    this.internal.setLocalState(initialState())
    this.uploadingFiles = reactive(new Map())

    this.internal.on('update', (updates: AwarenessUpdates) => {
      updates.removed.forEach((id) => this.uploadingFiles.delete(id))
      for (const id of [...updates.added, ...updates.updated]) {
        const uploads = this.internal.getStates().get(id)?.uploads
        if (uploads) {
          this.uploadingFiles.set(id, structuredClone(uploads))
        }
      }
    })
  }

  /** TODO: Add docs */
  public addOrUpdateUpload(name: FileName, file: UploadingFile) {
    this.withUploads((uploads) => {
      uploads[name] = file
    })
  }

  /** TODO: Add docs */
  public removeUpload(name: FileName) {
    this.withUploads((uploads) => {
      delete uploads[name]
    })
  }

  /** TODO: Add docs */
  public allUploads(): Iterable<[FileName, UploadingFile]> {
    return [...this.uploadingFiles.values()].flatMap((uploads) => [...Object.entries(uploads)])
  }

  private withUploads(f: (uploads: Uploads) => void) {
    const state = this.internal.getLocalState() as State
    f(state.uploads)
    this.internal.setLocalState(state)
  }
}

// === Private types ===

type ClientId = number

interface State {
  uploads: Uploads
}

type Uploads = Record<FileName, UploadingFile>

const initialState: () => State = () => ({ uploads: {} })

interface AwarenessUpdates {
  added: ClientId[]
  removed: ClientId[]
  updated: ClientId[]
}
