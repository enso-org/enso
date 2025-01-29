import { linkEditPopup } from '@/util/codemirror/linkEditPopup'
import {
  linkAttributesFactory,
  linkAttributesFactoryChanged,
  linkDecoratorStateExt,
} from '@/util/codemirror/links'
import { LINKABLE_EMAIL_REGEX, LINKABLE_URL_REGEX } from '@/util/link'
import { RangeSetBuilder, type Extension } from '@codemirror/state'
import {
  Decoration,
  ViewPlugin,
  type DecorationSet,
  type EditorView,
  type ViewUpdate,
} from '@codemirror/view'

const viewPlugin = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet

    constructor(view: EditorView) {
      this.decorations = decorate(view)
    }

    update(update: ViewUpdate) {
      if (update.docChanged || update.viewportChanged || linkAttributesFactoryChanged(update)) {
        this.decorations = decorate(update.view)
      }
    }
  },
  {
    decorations: (v) => v.decorations,
  },
)

interface RangeLike<T> {
  from: number
  to: number
  value: T
}

function regexpMatcher<T>(
  regexp: RegExp,
  matchHandler: (match: RegExpExecArray) => T,
): (text: string) => Iterable<RangeLike<T>> {
  function* matcher(text: string) {
    for (const match of text.matchAll(regexp)) {
      const from = match.index
      const to = from + match[0].length
      const value = matchHandler(match)
      yield { from, to, value }
    }
  }
  return matcher
}

const MATCHERS = [
  regexpMatcher(LINKABLE_URL_REGEX, (match) =>
    match[0].startsWith('http') ? match[0] : `https://${match[0]}`,
  ),
  regexpMatcher(LINKABLE_EMAIL_REGEX, (match) => `mailto:${match[0]}`),
]

function decorate(view: EditorView): DecorationSet {
  const makeAttributes = view.state.facet(linkAttributesFactory)
  if (!makeAttributes) return Decoration.none
  const decorations = new RangeSetBuilder<Decoration>()
  for (const visibleRange of view.visibleRanges) {
    const visibleText = view.state.doc.sliceString(visibleRange.from, visibleRange.to)
    for (const matcher of MATCHERS) {
      for (const match of matcher(visibleText)) {
        decorations.add(
          visibleRange.from + match.from,
          visibleRange.from + match.to,
          Decoration.mark({
            tagName: 'a',
            attributes: makeAttributes(match.value),
          }),
        )
      }
    }
  }
  return decorations.finish()
}

/** Displays a popup when the cursor is inside a URL. */
const urlEditPopup = linkEditPopup(
  (el) => (el instanceof HTMLAnchorElement ? el.href : undefined),
  { popOut: true },
)

/** CodeMirror extension rendering URLs and email addresses as links. */
export const linkifyUrls: Extension = [linkDecoratorStateExt, viewPlugin, urlEditPopup]
