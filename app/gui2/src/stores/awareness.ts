import { Vec2 } from '@/util/vec2'
import type { StackItem } from 'shared/languageServerTypes'
import { reactive } from 'vue'
import { Awareness as YjsAwareness } from 'y-protocols/awareness'
import * as Y from 'yjs'

interface State {
  uploads: Uploads
}
type ClientId = number
export type FileName = string
const initialState: () => State = () => ({ uploads: {} })

export class Awareness {
  public internal: YjsAwareness
  uploadingFiles: Map<ClientId, Uploads>

  constructor(doc: Y.Doc) {
    this.internal = new YjsAwareness(doc)
    this.internal.setLocalState(initialState())
    this.uploadingFiles = reactive(new Map())

    this.internal.on('update', (updates: AwarenessUpdates) => {
      updates.removed.forEach((id) => this.uploadingFiles.delete(id))
      for (const id of [...updates.added, ...updates.updated]) {
        const uploads = this.internal.getStates().get(id)?.uploads
        if (uploads) {
          this.uploadingFiles.set(id, uploads)
        }
      }
    })
  }

  public addOrUpdateUpload(name: FileName, file: UploadingFile) {
    this.withUploads((uploads) => {
      uploads[name] = file
    })
  }

  public removeUpload(name: FileName) {
    this.withUploads((uploads) => {
      delete uploads[name]
    })
  }

  public allUploads(): Iterable<[FileName, UploadingFile]> {
    return [...this.uploadingFiles.values()].flatMap((uploads) => [...Object.entries(uploads)])
  }

  withUploads(f: (uploads: Uploads) => void) {
    const state = this.internal.getLocalState() as State
    f(state.uploads)
    this.internal.setLocalState(state)
  }
}

interface AwarenessUpdates {
  added: ClientId[]
  removed: ClientId[]
  updated: ClientId[]
}

export interface UploadingFile {
  sizePercentage: number
  stackItem: StackItem
  position: Vec2
}

type Uploads = Record<FileName, UploadingFile>
