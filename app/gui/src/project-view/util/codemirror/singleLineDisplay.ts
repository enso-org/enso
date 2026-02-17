import { RangeSetBuilder, StateField, type EditorState, type Extension } from '@codemirror/state'
import { Decoration, EditorView, WidgetType, type DecorationSet } from '@codemirror/view'

class PlainTextWidget extends WidgetType {
  private element: HTMLElement | undefined

  /** Constructor. */
  constructor(
    protected readonly className: string,
    protected readonly text: string,
  ) {
    super()
  }

  /** See {@link WidgetType.estimatedHeight}. */
  override get estimatedHeight() {
    return 1
  }

  /** See {@link WidgetType.toDOM}. */
  override toDOM(): HTMLElement {
    if (!this.element) {
      const container = document.createElement('span')
      container.className = this.className
      container.textContent = this.text
      this.element = container
    }
    return this.element
  }

  /** See {@link WidgetType.destroy}. */
  override destroy() {
    this.element = undefined
  }
}

const lineBreakPlaceholder = () => new PlainTextWidget('cm-linebreak-placeholder', '\u21B5')
function makeDecos(state: EditorState) {
  return decorateLinebreaks(state, () =>
    Decoration.replace({
      widget: lineBreakPlaceholder(),
    }),
  )
}

const ext = StateField.define<DecorationSet>({
  create(state) {
    return makeDecos(state)
  },
  update(prev, tr) {
    if (!tr.docChanged) return prev
    return makeDecos(tr.state)
  },
  provide: (f) => EditorView.decorations.from(f),
})

/** @internal */
export function decorateLinebreaks(state: EditorState, decorate: () => Decoration) {
  if (!state.doc.length) return Decoration.none
  const builder = new RangeSetBuilder<Decoration>()
  let pos = 0
  const iter = state.doc.iter()
  while (!iter.done) {
    const from = pos
    pos += iter.value.length
    if (iter.lineBreak) builder.add(from, pos, decorate())
    iter.next()
  }
  return builder.finish()
}

/** CodeMirror extension that renders linebreaks as an inline placeholder character. */
export function singleLineDisplay(): Extension {
  return ext
}
