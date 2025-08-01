import type { PersistableStatePlugin } from '@/util/codemirror/persistence/persistableStatePlugin'
import { EditorSelection, StateEffect } from '@codemirror/state'
import { EditorView, type PluginValue, ViewPlugin, type ViewUpdate } from '@codemirror/view'
import { shallowRef, type ShallowRef, triggerRef } from 'vue'
import { z } from 'zod'

const scrollModeSchema = z.union([
  z.literal('nearest'),
  z.literal('start'),
  z.literal('end'),
  z.literal('center'),
])
const scrollAxisSchema = z.object({
  mode: scrollModeSchema,
  margin: z.number(),
})
const scrollSchema = z.object({
  range: z.object({
    anchor: z.number(),
    head: z.number().optional(),
  }),
  x: scrollAxisSchema.optional(),
  y: scrollAxisSchema.optional(),
})

export type ScrollState = z.infer<typeof scrollSchema>

type ScrollTarget = ReturnType<EditorView['scrollSnapshot']>['value']
const scrollRestoreStateEffect = StateEffect.define()

export interface ScrollStatePluginOptions {
  /** Whether to persist the scroll position along the y-axis. */
  y: boolean
  /**
   * Whether to persist the scroll position along the x-axis. Note that, if the view is not
   * scrollable on the x-axis, this MUST be `false`. If it is `true` but the editor's element is not
   * horizontally scrollable, restoring state may result in scrolling an ancestor of the editor.
   */
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
    const { range, y, x, yMargin, xMargin } = snapshot
    return {
      range: {
        anchor: range.anchor,
        head: range.head,
      },
      ...(this.y ?
        {
          y: {
            mode: y,
            margin: yMargin,
          },
        }
      : {}),
      ...(this.x ?
        {
          x: {
            mode: x,
            margin: xMargin,
          },
        }
      : {}),
    }
  }

  restoreState(rawState: unknown) {
    const state = ScrollStatePlugin.parseState(rawState)
    if (!state) return
    const { range, y, x } = state
    this.restoringScroll = true
    try {
      const selectionRange = EditorSelection.range(range.anchor, range.head ?? range.anchor)
      const scrollOptions = {
        isSnapshot: true,
        ...(this.y && y ?
          {
            y: y.mode,
            yMargin: y.margin,
          }
        : {}),
        ...(this.x && x ?
          {
            x: x.mode,
            xMargin: x.margin,
          }
        : {}),
      }
      this.view.dispatch({
        effects: [
          EditorView.scrollIntoView(selectionRange, scrollOptions),
          scrollRestoreStateEffect.of(null),
        ],
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
}

export const scrollStatePlugin = ViewPlugin.fromClass(ScrollStatePlugin, {
  // CM doesn't run `scrollEnd`, so we have to use `scroll` even though it's inefficient.
  eventObservers: { scroll: ScrollStatePlugin.prototype.onScroll },
})
