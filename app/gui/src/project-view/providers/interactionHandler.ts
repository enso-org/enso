import { createContextStore } from '@/providers'
import { shallowRef, toRaw, watch, type WatchSource } from 'vue'

export const [provideInteractionHandler, injectInteractionHandler] = createContextStore(
  'Interaction handler',
  () => new InteractionHandler(),
)

/** A storage and controller for interactions. */
export class InteractionHandler {
  private currentInteraction = shallowRef<Interaction>()

  /**
   * Check if given interaction is the current interaction.
   * It does not consider the whole interaction stack, only the last one.
   */
  isActive(interaction: Interaction | undefined) {
    return interaction != null && interaction === this.currentInteraction.value
  }

  /** Automatically activate/deactivate specified interaction by the specified condition. */
  setWhen(active: WatchSource<boolean>, interaction: Interaction) {
    watch(active, (active) => {
      if (active) {
        this.setCurrent(interaction)
      } else {
        this.end(interaction)
      }
    })
  }

  /**
   * Same as {@link setWhen}, but provides a callback which will be called with the currently active interaction.
   *
   * It is roughly the same as `setWhen(active, interactionBuider(handler.getCurrent()))`.
   */
  setWhenWithParent(
    active: WatchSource<boolean>,
    interactionBuilder: (parent: Interaction | undefined) => Interaction,
  ) {
    watch(active, (active, _old, onCleanup) => {
      if (active) {
        const interaction = interactionBuilder(this.getCurrent())
        this.setCurrent(interaction)
        onCleanup(() => {
          this.end(toRaw(interaction))
        })
      }
    })
  }

  /** Set the current interaction. Any existing interactions will be ended (not cancelled). */
  setCurrent(interaction: Interaction | undefined) {
    if (!this.isActive(interaction)) {
      if (interaction?.parentInteraction !== this.currentInteraction.value) {
        this.endAll()
      }
      this.currentInteraction.value = interaction
    }
  }

  /** Get currently active interaction. */
  getCurrent(): Interaction | undefined {
    return this.currentInteraction.value
  }

  /** Clear the current interaction without calling any callback, if the current interaction is `interaction`. */
  ended(interaction: Interaction) {
    if (this.isActive(interaction)) this.currentInteraction.value = undefined
  }

  /**
   * End the interaction, if it is the current interaction or its ancestor.
   * Any children interactions of the given interaction will be ended as well.
   */
  end(interaction: Interaction) {
    search(this.currentInteraction.value, interaction, (found, children) => {
      this.currentInteraction.value = found.parentInteraction
      for (const interaction of children) {
        interaction.end?.()
      }
      found.end?.()
    })
  }

  /** End all interactions. */
  private endAll() {
    let current = this.currentInteraction.value
    while (current != null) {
      current.end?.()
      current = current.parentInteraction
    }
    this.currentInteraction.value = undefined
  }

  /**
   * Cancel the interaction, if it is currently active.
   * Any children interactions of the given interaction will be cancelled as well=.
   */
  cancel(interaction: Interaction) {
    search(this.currentInteraction.value, interaction, (found, children) => {
      this.currentInteraction.value = found.parentInteraction
      for (const interaction of children) {
        interaction.cancel?.()
      }
      found.cancel?.()
    })
  }

  /**
   * Cancel all interactions.
   * @returns `true` if the current interaction was cancelled.
   */
  cancelAll(): boolean {
    const hasCurrent = this.currentInteraction.value != null
    let interaction = this.currentInteraction.value
    while (interaction != null) {
      interaction.cancel?.()
      interaction = interaction.parentInteraction
    }
    this.currentInteraction.value = undefined
    return hasCurrent
  }

  /**
   * Handle pointer event in capture. Calls the corresponding handler of the currently active interaction.
   *
   * Because usually the handlers check for clicks outside the active panel, even if event is handled,
   * it is NOT stopped, and its default action is NOT prevented.
   *
   * Only the current interaction is considered, its ancestors are not notified.
   */
  handlePointerDown(event: PointerEvent): boolean {
    if (!this.currentInteraction.value) return false
    const handler = this.currentInteraction.value.pointerdown
    if (!handler) return false
    return handler.bind(this.currentInteraction.value)(event) !== false
  }
}

/**
 * Search interactions stack.
 * @param stack - Stack to traverse.
 * @param needle - Interaction to find.
 * @param whenFound - Callback to call when the needle is found.
 */
function search(
  stack: Interaction | undefined,
  needle: Interaction,
  whenFound: (current: Interaction, children: Interaction[]) => void,
) {
  const children = []
  let current = stack
  while (current != null && current !== needle) {
    children.push(current)
    current = current.parentInteraction
  }
  if (current === needle) {
    whenFound(current, children)
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
  /** Parent interaction, to support nesting. */
  parentInteraction?: Interaction | undefined
}
