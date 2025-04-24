import { textEditorsBindings, textEditorsMultilineBindings } from '@/bindings'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { type VueHost } from '@/components/VueHostRender.vue'
import { injectKeyboard } from '@/providers/keyboard'
import {
  contentFocused,
  contentFocusedExt,
  setContentFocused,
} from '@/util/codemirror/contentFocusedExt'
import { baseKeymap, handlerToKeyBinding, verticalMovementKeymap } from '@/util/codemirror/keymap'
import { useCompartment, useDispatch, useStateEffect } from '@/util/codemirror/reactivity'
import { setVueHost } from '@/util/codemirror/vueHostExt'
import { yCollab } from '@/util/codemirror/yCollab'
import { elementHierarchy } from '@/util/dom'
import { type ToValue } from '@/util/reactivity'
import { insertNewlineKeepIndent } from '@codemirror/commands'
import {
  Compartment,
  EditorState,
  type Extension,
  Prec,
  type SelectionRange,
  type StateEffect,
  type StateEffectType,
  Text,
  Transaction,
} from '@codemirror/state'
import { EditorView, type KeyBinding, keymap, placeholder } from '@codemirror/view'
import { LINE_BOUNDARIES } from 'enso-common/src/utilities/data/string'
import {
  type ComponentInstance,
  computed,
  isRef,
  onUnmounted,
  toValue,
  watchEffect,
  type WatchSource,
} from 'vue'
import { Awareness } from 'y-protocols/awareness.js'
import { assert } from 'ydoc-shared/util/assert'
import { Range } from 'ydoc-shared/util/data/range'
import * as Y from 'yjs'

function disableEditContextApi() {
  ;(EditorView as any).EDIT_CONTEXT = false
}

/* Disable EditContext API because of https://github.com/codemirror/dev/issues/1458. */
disableEditContextApi()

export type Getter<T> = () => T

/** Creates a CodeMirror editor instance, and sets its initial state. */
export function useCodeMirror(
  editorRoot: ToValue<ComponentInstance<typeof CodeMirrorRoot> | null>,
  {
    content,
    placeholder: placeholderText,
    extensions,
    vueHost,
    contentTestId,
    readonly: isReadonly,
    lineMode,
  }: {
    /** If a value is provided, the editor state will be synchronized with it. */
    content?: ToValue<string | Y.Text>
    placeholder?: ToValue<string>
    /** CodeMirror {@link Extension}s to include in the editor's initial state. */
    extensions?: Extension
    /**
     * If a value is provided, it will be made available to extensions that render Vue components.
     */
    vueHost?: WatchSource<VueHost | undefined>
    /** If provided, the element with class `cm-content` will also have the given `data-testid`. */
    contentTestId?: string | undefined
    readonly?: boolean
    lineMode?: ToValue<'single' | 'multi' | 'auto'>
  },
) {
  const view = new EditorView()
  onUnmounted(view.destroy.bind(view))
  if (contentTestId != null) view.contentDOM.dataset['testid'] = contentTestId
  const readonly = computed(
    () => isReadonly ?? (!!content && !isRef(content) && typeof toValue(content) === 'string'),
  )
  const readonlyExt = useCompartment(view, () =>
    toValue(readonly) ? [EditorState.readOnly.of(true), EditorView.editable.of(false)] : [],
  )
  const placeholderExt =
    placeholderText ? useCompartment(view, () => placeholder(toValue(placeholderText))) : []
  const { bindingsExt } = useBindings(view)
  const sync = content ? useYTextOrReadonlySync(content) : undefined
  const extrasCompartment = new Compartment()
  const bindingsCompartment = useCompartment(view, () =>
    keyBindings(view, { lineMode: toValue(lineMode) }),
  )
  const singleLineState = computed(() => {
    const mode = toValue(lineMode)
    return mode && mode !== 'multi'
  })
  const themeCompartment = useCompartment(view, () => theme({ singleLine: singleLineState.value }))
  view.setState(
    EditorState.create({
      extensions: [
        readonlyExt,
        bindingsExt,
        placeholderExt,
        bindingsCompartment,
        themeCompartment,
        sync?.syncExt ?? [],
        extrasCompartment.of([]),
        extensions ?? [],
      ],
    }),
  )
  if (vueHost) useStateEffect(view, setVueHost, vueHost)
  sync?.connectSync(view)

  watchEffect(() => {
    const editorRootValue = toValue(editorRoot)
    if (editorRootValue) editorRootValue.$el.prepend(view.dom)
  })

  /**
   * Replace text in given document range with `text`, putting text cursor after inserted text.
   */
  function putTextAt(text: string, from: number, to: number) {
    const insert = Text.of(text.split(LINE_BOUNDARIES))
    view.dispatch({
      changes: { from, to, insert },
      selection: { anchor: from + insert.length },
    })
  }

  return {
    /** The {@link EditorView}, connecting the current state with the DOM. */
    editorView: view,
    /**
     * Update a set of additional extensions for the editor.
     *
     * This function can be used to provide extensions that are not ready before `useCodeMirror` can
     * be called, e.g. because they require an {@link EditorView} instance to be created. If called
     * more than once, the new collection of extra extensions will replace the previous collection.
     *
     * The change will be dispatched asynchronously; this avoids observing an inconsistent state:
     * When an extension is removed, its event handlers may still fire if they were triggered in the
     * same tick (i.e. by the same event that caused the extension to be removed); in that case, the
     * handler would likely misbehave due to its extension not being installed, and all its state
     * fields being missing.
     *
     * Delaying any extension changes ensures that, when removing an extension, it is in a valid
     * state while handling the event that removed it; and, while adding an extension, it doesn't
     * handle the event that caused its installation before it is ready.
     */
    setExtraExtensions: (extensions: Extension | undefined) =>
      setTimeout(() =>
        view.dispatch({
          effects: extrasCompartment.reconfigure(extensions ?? []),
        }),
      ),
    /**
     * When `useCodeMirror` is configured to set up synchronization by passing the `content`
     * argument, this value tracks whether the content synchronized with the document is writable.
     */
    readonly,
    putTextAt,
    /** The DOM element containing the editor's content. */
    contentElement: view.contentDOM,
  }
}

function useBindings(view: EditorView) {
  const keyboard = injectKeyboard(true)

  function openLink(event: Event) {
    let element: HTMLAnchorElement | undefined = undefined
    for (const el of elementHierarchy(event.target)) {
      if (el instanceof HTMLAnchorElement) {
        element = el
        break
      }
      if (el === view.contentDOM) break
    }
    if (!element) return false
    event.preventDefault()
    event.stopPropagation()
    window.open(element.href, '_blank', 'noopener,noreferrer')
    return true
  }

  const bindingsHandler = textEditorsBindings.handler({
    openLink,
  })
  return {
    bindingsExt: EditorView.domEventHandlers({
      keydown: (event) => bindingsHandler(event),
      click: (event) => bindingsHandler(event) || (view.state.readOnly && openLink(event)),
      pointerdown: (event) => {
        keyboard?.updateState(event)
        if (keyboard?.mod) event.preventDefault()
      },
    }),
  }
}

/**
 * Creates a CodeMirror extension for reading, writing, and watching the editor's contents as a
 * string value.
 */
export function useStringSync() {
  const textEditCallbacks: ((text: string) => void)[] = []
  const userActionCallbacks: ((text: string, selection: SelectionRange) => void)[] = []
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
        for (const cb of userActionCallbacks) cb(text, update.state.selection.main)
        if (textEdit) for (const cb of textEditCallbacks) cb(text)
      }
    }),
    connectSync: (view: EditorView) => {
      function getText(): string {
        return view.state.doc.toString()
      }

      function setText(text: string, selection?: Range): void {
        const safeSelection = selection?.clip(Range.fromStartAndLength(0, text.length))
        if (selection && !selection.rangeEquals(safeSelection))
          console.warn('Clipping invalid selection', { text, selection })
        view.dispatch({
          changes: { from: 0, to: view.state.doc.length, insert: text },
          selection:
            safeSelection ? { anchor: safeSelection.from, head: safeSelection.to } : { anchor: 0 },
        })
      }

      function onTextEdited(callback: (text: string) => void): void {
        textEditCallbacks.push(callback)
      }

      function onUserAction(callback: (text: string, selection: SelectionRange) => void): void {
        userActionCallbacks.push(callback)
      }

      return {
        getText,
        setText,
        onTextEdited,
        onUserAction,
      }
    },
  }
}

function useYTextOrReadonlySync(content: ToValue<string | Y.Text>) {
  const syncCompartment = new Compartment()
  const awareness = new Awareness(new Y.Doc())

  function sync() {
    const contentValue = toValue(content)
    if (typeof contentValue === 'string') {
      return { text: contentValue, extensions: [] }
    } else {
      assert(contentValue.doc !== null)
      const yTextWithDoc: Y.Text & { doc: Y.Doc } = contentValue as any
      return { text: contentValue.toString(), extensions: yCollab(yTextWithDoc, awareness) }
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
    syncExt: syncCompartment.of([]),
    connectSync: (view: EditorView) =>
      useDispatch(
        view,
        () => applySync(view.state, sync()),
        // The y-sync plugin breaks if it is reconfigured directly (it never unobserves the original
        // yText), but can handle being removed and reinstalled.
        () =>
          view.dispatch({
            effects: syncCompartment.reconfigure([]),
          }),
      ),
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

const stopEvent = (event: Event) => {
  event.stopImmediatePropagation()
  return false
}
function bindStandardBindings(view: EditorView) {
  const autoOrMultiHandlers = handlerToKeyBinding(
    textEditorsMultilineBindings.handler({
      newline: (e) => {
        e.stopImmediatePropagation()
        return insertNewlineKeepIndent(view)
      },
    }),
  )
  return {
    multiline: [autoOrMultiHandlers, ...verticalMovementKeymap(view)] satisfies KeyBinding[],
    singleline: [],
    autoline: [autoOrMultiHandlers] satisfies KeyBinding[],
  }
}

function keyBindings(
  view: EditorView,
  { lineMode }: { lineMode?: 'single' | 'multi' | 'auto' | undefined } = {},
): Extension {
  const mode = lineMode ?? 'multi'
  const standardBindings = bindStandardBindings(view)
  return [
    Prec.lowest(keymap.of(baseKeymap(view))),
    Prec.low(
      keymap.of(
        mode === 'multi' ? standardBindings.multiline
        : mode === 'auto' ? standardBindings.autoline
        : standardBindings.singleline,
      ),
    ),
    ...(mode === 'multi' ?
      [
        EditorView.domEventHandlers({
          wheel: stopEvent,
        }),
      ]
    : []),
  ]
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
