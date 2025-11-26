import { LINE_BOUNDARIES } from '$/utils/data/string'
import { textEditorsBindings } from '@/bindings'
import type CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import type { VueHost } from '@/components/VueHostRender.vue'
import { injectKeyboard } from '@/providers/keyboard'
import { usePopoverRoot } from '@/providers/popoverRoot'
import {
  contentFocused,
  contentFocusedExt,
  setContentFocused,
} from '@/util/codemirror/contentFocusedExt'
import { extendCmEvent, keyBindings, type CmEventExt } from '@/util/codemirror/keymap'
import { useCompartment, useDispatch, useStateEffect } from '@/util/codemirror/reactivity'
import { setVueHost, vueHostExt } from '@/util/codemirror/vueHostExt'
import { yCollab } from '@/util/codemirror/yCollab'
import type { Vec2 } from '@/util/data/vec2'
import { elementHierarchy } from '@/util/dom'
import type { ToValue } from '@/util/reactivity'
import type { AnyHandlerEvent } from '@/util/shortcuts'
import {
  Compartment,
  EditorState,
  Text,
  Transaction,
  type Extension,
  type SelectionRange,
  type StateEffect,
  type StateEffectType,
  type TransactionSpec,
} from '@codemirror/state'
import { EditorView, placeholder, tooltips } from '@codemirror/view'
import { find, takeUntil } from 'enso-common/src/utilities/data/iter'
import { createDebouncer } from 'lib0/eventloop.js'
import {
  computed,
  isRef,
  markRaw,
  onUnmounted,
  readonly,
  ref,
  toValue,
  watch,
  type ComponentInstance,
  type WatchSource,
} from 'vue'
import { Awareness } from 'y-protocols/awareness.js'
import { assert } from 'ydoc-shared/util/assert'
import { Range } from 'ydoc-shared/util/data/range'
import type { LocalUserActionOrigin } from 'ydoc-shared/yjsModel'
import * as Y from 'yjs'

function disableEditContextApi() {
  ;(EditorView as any).EDIT_CONTEXT = false
}

/* Disable EditContext API because of https://github.com/codemirror/dev/issues/1458. */
disableEditContextApi()

export type LineMode = 'single' | 'multi' | 'auto' | 'autoMulti'

export type Getter<T> = () => T

interface CodeMirrorOptions {
  placeholder?: ToValue<string>
  /**
   * CodeMirror extensions to include in the editor's state. Values may be {@link Extension}s, or
   * {@link WatchSource}s that return {@link Extension}s; in the latter case, a compartment will be
   * created for each watch source to allow its contents to be reactively reconfigured.
   */
  extensions?: (Extension | WatchSource<Extension>)[]
  /**
   * If a value is provided, it will be made available to extensions that render Vue components.
   */
  vueHost?: WatchSource<VueHost | undefined>
  /** If provided, the element with class `cm-content` will also have the given `data-testid`. */
  contentTestId?: string | undefined
  /** If provided, the element with class `cm-scroller` will also have the given `data-testid`. */
  scrollerTestId?: string | undefined
  readonly?: ToValue<boolean>
  lineMode: ToValue<LineMode>
}

/**
 * Creates a CodeMirror editor instance.
 *
 * The editor will be empty. To set and synchronize its contents, use proper extension, like
 * {@link useStringSync}, {@link yCollab}, or {@link useYTextSync}. If they require {@link EditorView},
 * they may be attached with `setExtraExtensions` method.
 */
export function useCodeMirror(
  editorRoot: ToValue<ComponentInstance<typeof CodeMirrorRoot> | null>,
  {
    placeholder: placeholderText,
    extensions,
    vueHost,
    contentTestId,
    scrollerTestId,
    readonly,
    lineMode,
  }: CodeMirrorOptions,
) {
  const dispatch = { dispatch: (...specs: TransactionSpec[]) => view.dispatch(...specs) }
  const readonlyExt = useCompartment(dispatch, () =>
    toValue(readonly) ?
      [EditorState.readOnly.of(true), EditorView.editable.of(false)]
    : NULL_EXTENSION,
  )
  const placeholderExt =
    placeholderText ?
      useCompartment(dispatch, () => placeholder(toValue(placeholderText)))
    : NULL_EXTENSION
  const { bindingsExt } = useBindings()
  const extrasCompartment = markRaw(new Compartment())
  const bindingsCompartment = useCompartment(dispatch, () => keyBindings(toValue(lineMode)))
  const singleLineState = computed(() => {
    const mode = toValue(lineMode)
    return mode !== 'multi' && mode !== 'autoMulti'
  })
  const themeCompartment = useCompartment(dispatch, () =>
    theme({ singleLine: singleLineState.value }),
  )
  const popoverRoot = usePopoverRoot(true)
  const tooltipsConfigExt =
    popoverRoot == null ? NULL_EXTENSION : (
      useCompartment(dispatch, () =>
        popoverRoot.value == null ?
          NULL_EXTENSION
        : tooltips({
            position: 'absolute',
            parent: popoverRoot.value,
          }),
      )
    )

  const reactiveExtensions =
    extensions ?
      extensions.map((ext) =>
        isRef(ext) || typeof ext === 'function' ? useCompartment(dispatch, ext) : ext,
      )
    : NULL_EXTENSION
  const view = markRaw(
    new EditorView({
      state: EditorState.create({
        extensions: [
          readonlyExt,
          bindingsExt,
          placeholderExt,
          bindingsCompartment,
          themeCompartment,
          extrasCompartment.of(NULL_EXTENSION),
          reactiveExtensions,
          tooltipsConfigExt,
          vueHost ? vueHostExt : NULL_EXTENSION,
        ],
      }),
    }),
  )
  watch(
    () => toValue(editorRoot),
    (editorRootValue) => {
      if (editorRootValue) editorRootValue.$el.prepend(view.dom)
    },
    { immediate: true },
  )

  if (contentTestId != null) view.contentDOM.dataset['testid'] = contentTestId
  if (scrollerTestId != null) view.scrollDOM.dataset['testid'] = scrollerTestId
  onUnmounted(view.destroy.bind(view))

  if (vueHost) useStateEffect(view, setVueHost, vueHost)

  const extraExtsDebouncer = createDebouncer(0)

  return {
    /** The {@link EditorView}, connecting the current state with the DOM. */
    editorView: view,
    /**
     * Update a set of additional extensions for the editor.
     *
     * This function can be used to provide extensions that are not ready before `useCodeMirror` can
     * be called, e.g., because they require an {@link EditorView} instance to be created. If called
     * more than once, the new collection of extra extensions will replace the previous collection.
     *
     * The change will be dispatched asynchronously; this avoids observing an inconsistent state:
     * When an extension is removed, its event handlers may still fire if they were triggered in the
     * same tick (i.e., by the same event that caused the extension to be removed); in that case, the
     * handler would likely misbehave due to its extension not being installed, and all its state
     * fields being missing.
     *
     * Delaying any extension changes ensures that, when removing an extension, it is in a valid
     * state while handling the event that removed it; and, while adding an extension, it doesn't
     * handle the event that caused its installation before it is ready.
     */
    setExtraExtensions: (extensions: Extension | undefined) => {
      extraExtsDebouncer(() =>
        view.dispatch({
          effects: extrasCompartment.reconfigure(extensions ?? NULL_EXTENSION),
        }),
      )
    },
    /** The DOM element containing the editor's content. */
    contentElement: view.contentDOM,
  }
}

const NULL_EXTENSION: Extension = readonly([])

function useBindings() {
  const keyboard = injectKeyboard(true)

  function openLink(event: CmEventExt<AnyHandlerEvent>) {
    const parents = elementHierarchy(event.target)
    const inEditorHierarchy = takeUntil(parents, (el) => el === event.codemirrorView.contentDOM)
    const linkElement = find(inEditorHierarchy, (el) => el instanceof HTMLAnchorElement)
    if (!linkElement) return false

    event.preventDefault()
    event.stopPropagation()
    window.open(linkElement.href, '_blank', 'noopener,noreferrer')
    return true
  }

  const bindingsHandler = textEditorsBindings.handler({
    openLink,
  })
  return {
    bindingsExt: EditorView.domEventHandlers({
      click: (event, view) => {
        const cmEvent = extendCmEvent(view, event)
        return bindingsHandler(cmEvent) || (view.state.readOnly && openLink(cmEvent))
      },
      pointerdown: (event) => {
        keyboard?.updateState(event)
        if (keyboard?.mod) event.preventDefault()
      },
    }),
  }
}

interface StringSyncOptions {
  onTextEdited?: (text: string) => void
  onUserAction?: (text: string, selection: SelectionRange) => void
}

/**
 * Creates a CodeMirror extension for reading, writing, and watching the editor's contents as a
 * string value.
 */
export function useStringSync({ onTextEdited, onUserAction }: StringSyncOptions = {}) {
  return {
    syncExt: EditorView.updateListener.of((update) => {
      const textEdit = update.transactions.some(
        (transaction) => transaction.docChanged && transaction.annotation(Transaction.userEvent),
      )
      const userAction =
        textEdit ||
        update.transactions.some(
          (transaction) => transaction.selection && transaction.annotation(Transaction.userEvent),
        )
      if (userAction) {
        const text = update.state.doc.toString()
        if (onUserAction) onUserAction(text, update.state.selection.main)
        if (onTextEdited) onTextEdited(text)
      }
    }),
    getText: (view: EditorView): string => {
      return view.state.doc.toString()
    },
    setText: (view: EditorView, text: string, selection?: Range): void => {
      const safeSelection = selection?.clip(Range.fromStartAndLength(0, text.length))
      if (selection && !selection.rangeEquals(safeSelection))
        console.warn('Clipping invalid selection', { text, selection })
      view.dispatch({
        changes: { from: 0, to: view.state.doc.length, insert: text },
        selection:
          safeSelection ? { anchor: safeSelection.from, head: safeSelection.to } : { anchor: 0 },
      })
    },
  }
}

/** An extension synchronizing CM with a Y.Text node in the ref. */
export function useYTextSync(content: ToValue<Y.Text | undefined>, origin?: LocalUserActionOrigin) {
  const syncCompartment = new Compartment()
  const awareness = new Awareness(new Y.Doc())

  function sync() {
    const contentValue = toValue(content)
    if (contentValue != null) {
      assert(contentValue.doc !== null)
      const yTextWithDoc: Y.Text & { doc: Y.Doc } = contentValue as any
      return { text: contentValue.toString(), extensions: yCollab(yTextWithDoc, awareness, origin) }
    } else {
      return { text: '', extensions: [] }
    }
  }

  function setDocText(state: EditorState, text: string) {
    return { from: 0, to: state.doc.length, insert: text }
  }

  function applySync(
    state: EditorState,
    { text, extensions }: { text: string; extensions: Extension },
  ) {
    return {
      changes: setDocText(state, text),
      effects: syncCompartment.reconfigure(extensions),
    }
  }

  return {
    syncExt: syncCompartment.of(NULL_EXTENSION),
    connectSync: (view: EditorView) => {
      useDispatch(
        view,
        () => applySync(view.state, sync()),
        // The y-sync plugin breaks if it is reconfigured directly (it never unobserves the original
        // yText), but can handle being removed and reinstalled.
        () =>
          view.dispatch({
            effects: syncCompartment.reconfigure(NULL_EXTENSION),
          }),
      )
    },
  }
}

function lastEffect<T>(
  effects: ReadonlyArray<StateEffect<unknown>>,
  effectType: StateEffectType<T>,
): T | undefined {
  for (let i = effects.length - 1; i >= 0; i--) {
    const effect = effects[i]!
    if (effect.is(effectType)) return effect.value
  }
}

const baseTheme = EditorView.theme({
  '&.cm-editor': {
    display: 'contents',
    outline: 'none',
  },
  '.cm-scroller': {
    // The default is `monospace`, but even when we want the editor to be monospace we use more
    // specific fonts.
    'font-family': 'unset',
    // Prevent touchpad back gesture, which can be triggered while panning.
    'overscroll-behavior': 'none',
  },
})

const inlineTheme = EditorView.theme({
  '&.cm-editor': {
    margin: 0,
    'min-width': '1px',
  },
  '.cm-scroller': {
    display: 'contents',
  },
  '.cm-line': {
    padding: 0,
  },
})

const multilineTheme = EditorView.theme({
  '&.cm-editor': {
    position: 'relative',
    height: '100%',
    width: '100%',
    'text-align': 'left',
  },
})

function theme({ singleLine }: { singleLine?: boolean | undefined } = {}): Extension {
  return [baseTheme, singleLine ? inlineTheme : multilineTheme]
}

export const selectOnMouseFocus = [
  contentFocusedExt(),
  EditorState.transactionFilter.of((tr) => {
    if (tr.isUserEvent('select.pointer') && tr.startState.field(contentFocused) === false)
      return { selection: { anchor: 0, head: tr.startState.doc.length } }
    if (lastEffect(tr.effects, setContentFocused) === false)
      return [tr, { selection: { anchor: 0 } }]
    return tr
  }),
]

/**
 * Replace text in given document range with `text`, putting text cursor after inserted text.
 */
export function putTextAt(view: EditorView, text: string, from: number, to: number) {
  const insert = Text.of(text.split(LINE_BOUNDARIES))
  view.dispatch({
    changes: { from, to, insert },
    selection: { anchor: from + insert.length },
  })
}

/**
 * Insert text at cursor or replacing any selection, putting text cursor after inserted text.
 */
export function putText(view: EditorView, text: string) {
  const range = view.state.selection.main
  putTextAt(view, text, range.from, range.to)
}

/** Insert text at the given position in the editor. */
export function putTextAtCoords(view: EditorView, text: string, coords: Vec2) {
  const pos = view.posAtCoords(coords, false)
  putTextAt(view, text, pos, pos)
}

/**
 * @returns the editor's reactive focused state, maintained by attaching the returned event handlers
 * to the editor's root element.
 * This implements a focus state that differs from the DOM focus of any particular element. It
 * exhibits some hysteresis: When the scrollbar is clicked, the computed focus state doesn't change.
 * Thus, this should be used in lieu of the element's focus when the rendering of the editor's
 * content is focus-dependent in a way that may affect its size.
 */
export function useEditorFocus(view: EditorView) {
  const focused = ref(false)
  const focusHandlers = {
    focusin: (event: FocusEvent) => {
      // Enable rendering the line containing the current cursor in `editing` mode if focus enters
      // the element *inside* the scroll area--if we handled the event for the editor root, clicking
      // the scrollbar would cause editing mode to be activated.
      if (event.target instanceof Node && view.contentDOM.contains(event.target))
        focused.value = true
    },
    focusout: (event: FocusEvent) => {
      if (
        !(event.currentTarget instanceof Node) ||
        !(event.relatedTarget instanceof Node) ||
        !event.currentTarget?.contains(event.relatedTarget)
      ) {
        // If the focus leaves the whole editor, we exit editing mode. Note the asymmetry with
        // `onFocusIn`: This way, clicking the scrollbar doesn't change edit mode.
        focused.value = false
      }
    },
  }
  return { focused, focusHandlers }
}
