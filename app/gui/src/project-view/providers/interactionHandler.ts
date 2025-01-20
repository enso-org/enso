import { createContextStore } from '@/providers'
import { shallowRef, watch, type WatchSource } from 'vue'

export const [provideInteractionHandler, injectInteractionHandler] = createContextStore(
  'Interaction handler',
  () => new InteractionHandler(),
)

/** TODO: Add docs */
export class InteractionHandler {
  private currentInteraction = shallowRef<Interaction>()

  /** TODO: Add docs */
  isActive(interaction: Interaction | undefined): interaction is Interaction {
    return interaction != null && interaction === this.currentInteraction.value
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

  /** TODO: Add docs */
  setCurrent(interaction: Interaction | undefined) {
    if (!this.isActive(interaction)) {
      this.currentInteraction.value?.end?.()
      this.currentInteraction.value = interaction
    }
  }

  /** TODO: Add docs */
  getCurrent(): Interaction | undefined {
    return this.currentInteraction.value
  }

  /** Clear the current interaction without calling any callback, if the current interaction is `interaction`. */
  ended(interaction: Interaction) {
    if (this.isActive(interaction)) this.currentInteraction.value = undefined
  }

  /** End the current interaction, if it is the specified instance. */
  end(interaction: Interaction) {
    if (this.isActive(interaction)) {
      this.currentInteraction.value = undefined
      interaction.end?.()
    }
  }

  /** Cancel the current interaction, if it is the specified instance. */
  cancel(interaction: Interaction) {
    if (this.isActive(interaction)) {
      this.currentInteraction.value = undefined
      interaction.cancel?.()
    }
  }

  /** TODO: Add docs */
  handleCancel(): boolean {
    const hasCurrent = this.currentInteraction.value != null
    this.currentInteraction.value?.cancel?.()
    this.currentInteraction.value = undefined
    return hasCurrent
  }

  /** TODO: Add docs */
  handlePointerEvent<HandlerName extends keyof Interaction>(
    event: PointerEvent,
    handlerName: Interaction[HandlerName] extends InteractionEventHandler | undefined ? HandlerName
    : never,
  ): boolean {
    if (!this.currentInteraction.value) return false
    const handler = this.currentInteraction.value[handlerName]
    if (!handler) return false
    const handled = handler.bind(this.currentInteraction.value)(event) !== false
    if (handled) {
      event.stopImmediatePropagation()
      event.preventDefault()
    }
    return handled
  }
}

type InteractionEventHandler = (event: PointerEvent) => boolean | void

export interface Interaction {
  /** Called when the interaction is explicitly canceled, e.g. with the `Esc` key. */
  cancel?(): void
  /** Called when the interaction is ended due to activity elsewhere. */
  end?(): void
  /** Uses a `capture` event handler to allow an interaction to respond to clicks over any element. */
  pointerdown?: InteractionEventHandler
  /**
   * Uses a `capture` event handler to allow an interaction to respond to mouse button release
   * over any element. It is useful for interactions happening during mouse press (like dragging
   * edges)
   */
  pointerup?: InteractionEventHandler
}
