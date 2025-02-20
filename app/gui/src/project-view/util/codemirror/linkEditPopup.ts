import LinkEditPopup from '@/components/LinkEditPopup.vue'
import { type VueHost } from '@/components/VueHostRender.vue'
import {
  contentFocused,
  contentFocusedChanged,
  contentFocusedExt,
} from '@/util/codemirror/contentFocusedExt'
import { pseudoRef } from '@/util/codemirror/nonReactive'
import { getVueHost, vueHostChanged, vueHostExt } from '@/util/codemirror/vueHostExt'
import { elementHierarchy } from '@/util/dom'
import { type Extension } from '@codemirror/state'
import { type EditorView, layer, type LayerMarker, type ViewUpdate } from '@codemirror/view'
import { h, markRaw } from 'vue'

/** A CodeMirror extension that displays a popup when the cursor is inside a link. */
export function linkEditPopup(
  matchLink: (element: HTMLElement) => string | undefined,
  options: { popOut?: boolean } = {},
): Extension {
  const { updateMarker, currentMarker, markerDirty } = useLinkMarker(
    matchLink,
    options.popOut ?? false,
  )
  const popupExt = layer({
    above: true,
    markers: (view: EditorView) => {
      updateMarker(view)
      return currentMarker.value ? [currentMarker.value] : []
    },
    update: markerDirty,
  })
  return [contentFocusedExt(), vueHostExt, popupExt]
}

function useLinkMarker(matchLink: (element: HTMLElement) => string | undefined, popOut: boolean) {
  const currentMarker = pseudoRef<LinkMarker>()
  function setMarker(marker: LinkMarker | undefined) {
    const prevMarker = currentMarker.value
    if (!(prevMarker && marker && prevMarker.eq(marker))) {
      currentMarker.value = marker
      prevMarker?.destroy()
    }
  }
  function getNewMarker(view: EditorView) {
    const focused = view.state.facet(contentFocused)
    if (!focused) return
    const vueHost = view.state.facet(getVueHost)
    if (!vueHost) return
    const link = findCursorLink(view, matchLink)
    if (!link) return
    return new LinkMarker(link.element, link.href, vueHost, popOut)
  }
  return {
    updateMarker: (view: EditorView) => setMarker(getNewMarker(view)),
    currentMarker,
    markerDirty: (update: ViewUpdate) =>
      update.docChanged ||
      update.selectionSet ||
      update.viewportChanged ||
      contentFocusedChanged(update) ||
      vueHostChanged(update),
  }
}

function findCursorLink(
  view: EditorView,
  matchLink: (element: HTMLElement) => string | undefined,
): { element: HTMLElement; href: string } | undefined {
  const pos = view.state.selection.main.head
  if (!view.visibleRanges.some((range) => containsInclusive(range, pos))) return
  const { node } = view.domAtPos(pos)
  for (const el of elementHierarchy(node)) {
    if (el === view.contentDOM) break
    if (el instanceof HTMLElement) {
      const href = matchLink(el)
      if (href != null) return { element: el, href }
    }
  }
}

function containsInclusive(range: { from: number; to: number }, pos: number) {
  return range.from <= pos && pos <= range.to
}

class LinkMarker implements LayerMarker {
  private container: HTMLElement | undefined = undefined
  private vueHostRegistration: { unregister: () => void } | undefined = undefined

  constructor(
    private readonly element: HTMLElement,
    private readonly href: string,
    private readonly vueHost: VueHost,
    private readonly popOut: boolean,
  ) {}

  eq(other: LayerMarker) {
    return (
      other instanceof LinkMarker &&
      other.element === this.element &&
      other.href === this.href &&
      other.popOut === this.popOut
    )
  }

  draw() {
    if (!this.container) {
      const container = markRaw(document.createElement('div'))
      container.className = 'cm-link-edit-popup'
      this.vueHostRegistration = this.vueHost.register(
        h(LinkEditPopup, {
          referenceElement: this.element,
          href: this.href,
          popOut: this.popOut,
        }),
        container,
      )
      this.container = container
    }
    return this.container
  }

  destroy() {
    this.vueHostRegistration?.unregister()
    this.container = undefined
  }
}
