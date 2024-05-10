import { createContextStore } from '@/providers'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { watch, type WatchSource } from 'vue'

export { injectFn as injectInteractionHandler, provideFn as provideInteractionHandler }
const { provideFn, injectFn } = createContextStore(
  'Interaction handler',
  () => new InteractionHandler(),
)

export class InteractionHandler {
  private currentInteraction: Interaction | undefined = undefined

  isActive(interaction: Interaction | undefined): interaction is Interaction {
    return interaction != null && interaction === this.currentInteraction
  }

  /** Automatically activate specified interaction any time a specified condition becomes true. */
  setWhen(active: WatchSource<boolean>, interaction: Interaction) {
    watch(active, (active) => {
      if (active) {
        this.setCurrent(interaction)
      } else {
        this.end(interaction)
      }
    })
  }

  setCurrent(interaction: Interaction | undefined) {
    if (!this.isActive(interaction)) {
      this.currentInteraction?.end()
      this.currentInteraction = interaction
    }
  }

  getCurrent(): Interaction | undefined {
    return this.currentInteraction
  }

  /** Clear the current interaction without calling any callback, if the current interaction is `interaction`. */
  ended(interaction: Interaction) {
    if (this.isActive(interaction)) this.currentInteraction = undefined
  }

  /** End the current interaction, if it is the specified instance. */
  end(interaction: Interaction) {
    if (this.isActive(interaction)) {
      this.currentInteraction = undefined
      interaction.end()
    }
  }

  /** Cancel the current interaction, if it is the specified instance. */
  cancel(interaction: Interaction) {
    if (this.isActive(interaction)) {
      this.currentInteraction = undefined
      interaction.cancel()
    }
  }

  handleCancel(): boolean {
    const hasCurrent = this.currentInteraction != null
    this.currentInteraction?.cancel()
    this.currentInteraction = undefined
    return hasCurrent
  }

  handlePointerEvent<HandlerName extends keyof Interaction>(
    event: PointerEvent,
    handlerName: Interaction[HandlerName] extends InteractionEventHandler | undefined ? HandlerName
    : never,
    graphNavigator: GraphNavigator,
  ): boolean {
    if (!this.currentInteraction) return false
    const handler = this.currentInteraction[handlerName]
    if (!handler) return false
    const handled = handler.bind(this.currentInteraction)(event, graphNavigator) !== false
    if (handled) {
      event.stopImmediatePropagation()
      event.preventDefault()
    }
    return handled
  }
}

type InteractionEventHandler = (event: PointerEvent, navigator: GraphNavigator) => boolean | void

export interface Interaction {
  /** Called when the interaction is explicitly canceled, e.g. with the `Esc` key. */
  cancel(): void
  /** Called when the interaction is ended due to activity elsewhere. */
  end(): void
  /** Uses a `capture` event handler to allow an interaction to respond to clicks over any element. */
  pointerdown?: InteractionEventHandler
  /** Uses a `capture` event handler to allow an interaction to respond to mouse button release
   * over any element. It is useful for interactions happening during mouse press (like dragging
   * edges) */
  pointerup?: InteractionEventHandler
}
