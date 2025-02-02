import { textEditorsBindings } from '@/bindings'
import CodeMirrorRoot from '@/components/CodeMirrorRoot.vue'
import { type VueHost } from '@/components/VueHostRender.vue'
import { injectKeyboard } from '@/providers/keyboard'
import { useCompartment, useDispatch, useStateEffect } from '@/util/codemirror/reactivity'
import { setVueHost } from '@/util/codemirror/vueHostExt'
import { yCollab } from '@/util/codemirror/yCollab'
import { elementHierarchy } from '@/util/dom'
import { ToValue } from '@/util/reactivity'
import {
  ChangeDesc,
  ChangeSpec,
  Compartment,
  EditorState,
  type Extension,
  Text,
} from '@codemirror/state'
import { EditorView } from '@codemirror/view'
import { Tree } from '@lezer/common'
import { LINE_BOUNDARIES } from 'enso-common/src/utilities/data/string'
import {
  type ComponentInstance,
  computed,
  type Ref,
  toValue,
  watchEffect,
  type WatchSource,
} from 'vue'
import { Awareness } from 'y-protocols/awareness.js'
import { debugTree, markdownParser } from 'ydoc-shared/ast/ensoMarkdown'
import { assert } from 'ydoc-shared/util/assert'
import * as Y from 'yjs'

function disableEditContextApi() {
  ;(EditorView as any).EDIT_CONTEXT = false
}

/* Disable EditContext API because of https://github.com/codemirror/dev/issues/1458. */
disableEditContextApi()

/** Creates a CodeMirror editor instance, and sets its initial state. */
export function useCodeMirror(
  editorRoot: ToValue<ComponentInstance<typeof CodeMirrorRoot> | null>,
  {
    content,
    extensions,
    vueHost,
  }: {
    /** If a value is provided, the editor state will be synchronized with it. */
    content?: ToValue<string | Y.Text>
    /** CodeMirror {@link Extension}s to include in the editor's initial state. */
    extensions?: Extension
    /** If a value is provided, it will be made available to extensions that render Vue components. */
    vueHost?: WatchSource<VueHost | undefined>
  },
) {
  const editorView = new EditorView()
  const readonly = computed(() => !!content && typeof toValue(content) === 'string')
  const readonlyExt = useCompartment(editorView, () =>
    toValue(readonly) ? EditorState.readOnly.of(true) : [],
  )
  const { bindingsExt } = useBindings({ readonly, contentDOM: editorView.contentDOM })
  const sync = content ? useSync(content) : undefined
  const extrasCompartment = new Compartment()
  editorView.setState(
    EditorState.create({
      extensions: [
        readonlyExt,
        bindingsExt,
        sync?.syncExt ?? [],
        extrasCompartment.of([]),
        extensions ?? [],
      ],
    }),
  )
  if (vueHost) useStateEffect(editorView, setVueHost, vueHost)
  sync?.connectSync(editorView)

  watchEffect(() => {
    const editorRootValue = toValue(editorRoot)
    if (editorRootValue) editorRootValue.rootElement?.prepend(editorView.dom)
  })

  /**
   * Replace text in given document range with `text`, putting text cursor after inserted text.
   */
  function putTextAt(text: string, from: number, to: number) {
    const insert = Text.of(text.split(LINE_BOUNDARIES))
    editorView.dispatch({
      changes: { from, to, insert },
      selection: { anchor: from + insert.length },
    })
  }

  return {
    /** The {@link EditorView}, connecting the current state with the DOM. */
    editorView,
    /**
     * This function can be used to provide extensions that are not ready before `useCodeMirror` can be called, e.g.
     * because they require an {@link EditorView} instance to be created. If called more than once, the new collection
     * of extra extensions will replace the previous collection.
     */
    setExtraExtensions: (extensions: Extension) =>
      editorView.dispatch({
        effects: extrasCompartment.reconfigure([extensions]),
      }),
    /**
     * When `useCodeMirror` is configured to set up synchronization by passing the `content` argument, this value tracks
     * whether the content synchronized with the document is writable.
     */
    readonly,
    putTextAt,
    toggleHeader: (level: number) => toggleHeader(editorView, level),
    /** The DOM element containing the editor's content. */
    contentElement: editorView.contentDOM,
  }
}

export function toggleHeader(view: EditorView, level: number) {
  assert(level >= 1 && level <= 3, 'Invalid header level')
  const startLine = view.state.doc.lineAt(view.state.selection.main.from)
  const endLine = view.state.doc.lineAt(view.state.selection.main.to)
  const tree = markdownParser.parse(view.state.doc.toString())
  const addHeaders = []
  const replaceHeaders = []
  let removeHeaders = []
  for (let lineIndex = startLine.number; lineIndex <= endLine.number; lineIndex++) {
    const line = view.state.doc.line(lineIndex)
    const src = view.state.doc.toString()
    const result = toggleHeaderInner(tree, level, line.from, line.to, src)
    addHeaders.push(...result.add)
    replaceHeaders.push(...result.replace)
    removeHeaders.push(...result.remove)
  }
  if (addHeaders.length > 0) removeHeaders = []
  view.dispatch({ changes: [...addHeaders, ...removeHeaders, ...replaceHeaders] })
}

interface ToggleHeaderResult {
  add: ChangeSpec[]
  replace: ChangeSpec[]
  remove: ChangeSpec[]
}

function toggleHeaderInner(
  tree: Tree,
  level: number,
  lineStart: number,
  lineEnd: number,
  src: string,
): ToggleHeaderResult {
  const add = []
  const replace = []
  const remove = []
  const prefix = '#'.repeat(level)
  let nodeUnderCursor = tree.resolve(lineEnd, -1)
  if (nodeUnderCursor.type.name === 'Document' && nodeUnderCursor.firstChild != null)
    nodeUnderCursor = nodeUnderCursor.firstChild
  if (nodeUnderCursor.type.name.startsWith('ATXHeading')) {
    const headerMark = nodeUnderCursor.getChild('HeaderMark')
    if (headerMark) {
      // Heading text is everything after the header mark
      const headingText = src.slice(headerMark.to, lineEnd)
      if (nodeUnderCursor.type.name.endsWith(level.toString())) {
        remove.push({ from: lineStart, to: lineEnd, insert: headingText })
      } else {
        replace.push({ from: lineStart, to: lineEnd, insert: prefix + ' ' + headingText })
      }
    }
  } else if (nodeUnderCursor.type.name === 'CodeText') {
    const codeLine = src.slice(lineStart, lineEnd)
    const codeTree = markdownParser.parse(codeLine)
    const result = toggleHeaderInner(codeTree, level, lineStart, lineEnd, codeLine)
    add.push(...result.add)
    replace.push(...result.replace)
    remove.push(...result.remove)
  } else {
    add.push({ from: lineStart, to: lineEnd, insert: prefix + ' ' + src.slice(lineStart, lineEnd) })
  }
  return { add, replace, remove }
}

function useBindings({
  readonly,
  contentDOM,
}: {
  readonly: Readonly<Ref<boolean>>
  contentDOM: HTMLElement
}) {
  const keyboard = injectKeyboard()

  function openLink(event: Event) {
    let element: HTMLAnchorElement | undefined = undefined
    for (const el of elementHierarchy(event.target)) {
      if (el instanceof HTMLAnchorElement) {
        element = el
        break
      }
      if (el === contentDOM) break
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
      click: (event) => bindingsHandler(event) || (readonly.value && openLink(event)),
      pointerdown: (event) => {
        keyboard.updateState(event)
        if (keyboard.mod) event.preventDefault()
      },
    }),
  }
}

function useSync(content: ToValue<string | Y.Text>) {
  const syncCompartment = new Compartment()
  const awareness = new Awareness(new Y.Doc())

  function sync() {
    const contentValue = toValue(content)
    if (typeof contentValue === 'string') {
      return { text: contentValue, extensions: [] }
    } else {
      assert(contentValue.doc !== null)
      const yTextWithDoc: Y.Text & { doc: Y.Doc } = contentValue as any
      return { text: contentValue.toString(), extensions: [yCollab(yTextWithDoc, awareness)] }
    }
  }

  return {
    syncExt: syncCompartment.of([]),
    connectSync: (editorView: EditorView) => {
      function setDocText(text: string) {
        return { from: 0, to: editorView.state.doc.length, insert: text }
      }

      useDispatch(
        editorView,
        () => {
          const { text, extensions } = sync()
          return {
            changes: setDocText(text),
            effects: syncCompartment.reconfigure(extensions),
          }
        },
        // The y-sync plugin breaks if it is reconfigured directly (it never unobserves the original yText), but can
        // handle being removed and reinstalled.
        () =>
          editorView.dispatch({
            effects: syncCompartment.reconfigure([]),
          }),
      )
    },
  }
}
