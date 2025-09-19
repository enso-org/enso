import { useSyncLocalStorage } from '@/composables/syncLocalStorage'
import {
  scrollStatePlugin,
  type ScrollState,
  type ScrollStatePluginOptions,
} from '@/util/codemirror/persistence/scroll'
import type { Opt } from '@/util/data/opt'
import type { ToValue } from '@/util/reactivity'
import type { Extension } from '@codemirror/state'
import { EditorView, ViewPlugin, type PluginValue } from '@codemirror/view'
import { encoding } from 'lib0'
import { nextTick, readonly, shallowRef, toValue, type ShallowRef } from 'vue'
import { z } from 'zod'

const editorSchema = z.object({
  scroll: z.unknown(),
})

interface EditorState extends z.infer<typeof editorSchema> {
  scroll?: ScrollState
}

interface EditorPersistencePluginOptions {
  documentViewId: ToValue<Opt<string>>
}
/**
 * Plugin that synchronizes editor state with local storage; the state is aggregated from any
 * {@link PersistableStatePlugin}s (such as {@link scrollStatePlugin}) present in the view.
 */
class EditorPersistencePluginValue implements PluginValue {
  private readonly documentViewId: ToValue<Opt<string>>
  /**
   * This will be changed to `true` when the `isConnected` state is first observed and the
   * ready-callback scheduled.
   */
  private connected: boolean = false
  /**
   * This will be changed to `true` once the editor view is ready to be used. This prevents
   * attempting to read or write DOM state, such as scroll position, before the editor DOM has been
   * rendered.
   */
  private readonly ready: ShallowRef<boolean> = shallowRef(false)
  /**
   * Stores the last update attempted before the editor became ready, to be applied when the editor
   * becomes ready.
   */
  private deferredUpdate: (() => void) | undefined = undefined
  constructor(
    private readonly view: EditorView,
    { documentViewId }: EditorPersistencePluginOptions,
  ) {
    useSyncLocalStorage({
      storageKey: 'textEditor',
      mapKeyEncoder: (enc) =>
        encoding.writeVarString(enc, this.ready.value ? (toValue(this.documentViewId) ?? '') : ''),
      debounce: 200,
      captureState: this.captureState.bind(this),
      restoreState: this.restoreState.bind(this),
    })

    this.documentViewId = documentViewId
  }

  private onReady() {
    this.ready.value = true
    this.deferredUpdate?.()
    this.deferredUpdate = undefined
  }

  update() {
    if (this.view.contentDOM.isConnected && !this.connected) {
      this.connected = true
      nextTick(this.onReady.bind(this)).then()
    }
  }

  private captureState(): EditorState {
    if (!this.ready.value || !toValue(this.documentViewId)) return {}
    const scrollState = this.view.plugin(scrollStatePlugin)?.captureState()
    const scroll = scrollState ? readonly(scrollState) : undefined
    return {
      ...(scroll ? { scroll } : {}),
    }
  }

  private restoreState(rawState: unknown) {
    if (!toValue(this.documentViewId)) return
    if (!this.ready.value) {
      this.deferredUpdate = () => this.restoreState(rawState)
      return
    }
    this.deferredUpdate = undefined

    if (!rawState) return

    const state = editorSchema.safeParse(rawState)
    if (state.success) {
      if (state.data.scroll) this.view.plugin(scrollStatePlugin)?.restoreState(state.data.scroll)
    } else {
      console.warn('Failed to restore editor state', rawState, 'because', state.error.message)
    }
  }
}
const editorPersistencePlugin = ViewPlugin.fromClass<
  EditorPersistencePluginValue,
  EditorPersistencePluginOptions
>(EditorPersistencePluginValue)

export interface EditorPersistenceOptions {
  documentViewId: ToValue<Opt<string>>
  scroll?: ScrollStatePluginOptions
}

const NULL_EXTENSION: Extension = []

/**
 * Returns an extension that persists the specified aspects of the editor's state, associated with
 * the provided ID.
 */
export function editorPersistence({ documentViewId, scroll }: EditorPersistenceOptions): Extension {
  return [
    scroll ? scrollStatePlugin.of(scroll) : NULL_EXTENSION,
    editorPersistencePlugin.of({ documentViewId }),
  ]
}
