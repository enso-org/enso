import { watch, type WatchSource } from 'vue'
import { createContextStore } from '.'

export { injectFn as injectInteractionHandler, provideFn as provideInteractionHandler }
const { provideFn, injectFn } = createContextStore(
  'Interaction handler',
  () => new InteractionHandler(),
)

export class InteractionHandler {
  private currentInteraction: Interaction | undefined = undefined

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
    if (interaction !== this.currentInteraction) {
      this.currentInteraction?.cancel?.()
      this.currentInteraction = interaction
    }
  }

  /** Unset the current interaction, if it is the specified instance. */
  end(interaction: Interaction) {
    if (this.currentInteraction === interaction) {
      this.currentInteraction = undefined
    }
  }

  handleCancel(): boolean {
    const hasCurrent = this.currentInteraction != null
    if (hasCurrent) this.setCurrent(undefined)
    return hasCurrent
  }

  handleClick(event: MouseEvent): boolean | void {
    return this.currentInteraction?.click ? this.currentInteraction.click(event) : false
  }
}

export interface Interaction {
  cancel?(): void
  click?(event: MouseEvent): boolean | void
}
