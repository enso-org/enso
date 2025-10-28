import type { PersistableStatePlugin } from '@/util/codemirror/persistence/persistableStatePlugin'
import { EditorSelection, StateEffect } from '@codemirror/state'
import { EditorView, type PluginValue, ViewPlugin, type ViewUpdate } from '@codemirror/view'
import type { Mutable } from 'enso-common/src/utilities/data/object'
import { shallowRef, type ShallowRef, triggerRef } from 'vue'
import { z } from 'zod'

const documentRange = z.object({
  anchor: z.number(),
  head: z.number().optional(),
})

const scrollSchema = z.object({
  range: documentRange,
  x: z.number().optional(),
  y: z.number().optional(),
})

export type ScrollState = z.infer<typeof scrollSchema>

type DocumentRange = z.infer<typeof documentRange>
type ScrollTarget = ReturnType<EditorView['scrollSnapshot']>['value']
const scrollRestoreStateEffect = StateEffect.define()

export interface ScrollStatePluginOptions {
  /** Whether to persist the scroll position along the y-axis. */
  y: boolean
  /** Whether to persist the scroll position along the x-axis. */
  x: boolean
}

/** {@link PersistableStatePlugin} for editor scroll position. */
class ScrollStatePlugin implements PluginValue, PersistableStatePlugin<ScrollState> {
  private readonly scrollEffectRef: ShallowRef<undefined> = shallowRef()
  private restoringScroll: boolean = false
  private readonly x: boolean
  private readonly y: boolean
  constructor(
    private readonly view: EditorView,
    { x, y }: ScrollStatePluginOptions,
  ) {
    this.x = x
    this.y = y
  }

  update(update: ViewUpdate) {
    if (
      (update.docChanged || update.viewportChanged) &&
      !update.transactions.every((tr) => tr.effects.some((e) => e.is(scrollRestoreStateEffect)))
    )
      this.scrolled()
  }

  onScroll() {
    this.scrolled()
  }

  private scrolled() {
    if (!this.restoringScroll) triggerRef(this.scrollEffectRef)
  }

  captureState(): ScrollState {
    const _scrollEffect = this.scrollEffectRef.value
    return this.serializeState(this.view.scrollSnapshot().value)
  }

  private serializeState(snapshot: ScrollTarget): ScrollState {
    const { range, yMargin, xMargin } = snapshot
    return {
      range: {
        anchor: range.anchor,
        head: range.head,
      },
      y: this.y ? yMargin : undefined,
      x: this.x ? xMargin : undefined,
    }
  }

  restoreState(rawState: unknown) {
    const state = ScrollStatePlugin.parseState(rawState)
    if (!state) return
    const scrollTo = this.scrollSnapshotAt(state.range, state.y, state.x)
    if (!scrollTo) return
    this.restoringScroll = true
    try {
      this.view.dispatch({
        effects: [scrollTo, scrollRestoreStateEffect.of(null)],
      })
    } finally {
      this.restoringScroll = false
    }
  }

  private static parseState(rawState: unknown): ScrollState | undefined {
    const state = scrollSchema.safeParse(rawState)
    if (state.success) {
      return state.data
    } else {
      console.warn('Failed to restore scroll state', rawState, 'because', state.error.message)
    }
  }

  private scrollSnapshotAt(
    range: DocumentRange,
    y: number | undefined,
    x: number | undefined,
  ): StateEffect<unknown> {
    // CM does not offer a public API to create a "snapshot" scroll target with specific values; get
    // a current snapshot and overwrite its contents.
    const snapshot = this.view.scrollSnapshot()
    const snapshotValue = snapshot.value as Mutable<typeof snapshot.value>
    snapshotValue.range = EditorSelection.range(range.anchor, range.head ?? range.anchor)
    if (y != null) snapshotValue.yMargin = y
    if (x != null) snapshotValue.xMargin = x
    return snapshot
  }
}

export const scrollStatePlugin = ViewPlugin.fromClass(ScrollStatePlugin, {
  // CM doesn't run `scrollEnd`, so we have to use `scroll` even though it's inefficient.
  eventObservers: { scroll: ScrollStatePlugin.prototype.onScroll },
})
