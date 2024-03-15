import type { PortId } from '@/providers//portInfo'
import type { GraphNavigator } from '@/providers/graphNavigator'
import {
  injectInteractionHandler,
  type Interaction,
  type InteractionHandler,
} from '@/providers/interactionHandler'
import type { WidgetInput } from '@/providers/widgetRegistry'
import type { Ast } from '@/util/ast'

/** An extend {@link Interaction} used in {@link WidgetEditHandler} */
export interface WidgetEditInteraction extends Interaction {
  /** Click handler from {@link Interaction}, but received child click handler. See
   * {@link WidgetEditHandler} for details */
  click?(
    event: PointerEvent,
    navigator: GraphNavigator,
    childHandler?: () => boolean | void,
  ): boolean | void
  start?(origin: PortId): void
  edit?(origin: PortId, value: Ast.Owned | string): void
  end?(origin: PortId): void
}

/**
 * Widget edit handler.
 *
 * This handler takes an extended interaction and allows cooperation between parent/child
 * interactions. A usage example is WidgetSelection, which wants to open when the child is edited
 * and filters entries by edited temporary value.
 *
 * Widget's edit state should be manipulated by `start`, `end` and `cancel` methods; they will set
 * proper interaction in the global {@link InteractionHandler} and calls the additional callbacks in
 * {@link WidgetEditInteraction} passed during construction.
 *
 * The parent widget may pass its edit handler to one or more children's {@link WidgetInput} to
 * bound their interactions; when this child is edited, the parent is also considered edited,
 * along with any further ancestors. In particular:
 * - Starting, ending and cancelling (including automatic canceling by the global interaction
 *   handler) of child edit will also call proper callbacks in parent.
 * - Cancelling or ending parent edit will cancel/end the child's interaction.
 * - `isActive` method of both edit handlers will return true.
 *
 * This `edited` state is propagated only upwards: if only parent is edited, its children are not
 * considered edited. If child starts being edited while parent is still edited, the parent interaction
 * will be considered cancelled and then immediately started again. Similarly, when a parent's handler
 * is bound to two children, and one of them starts editing while the other is edited, the parent
 * will receive `cancel` feedback from the latter and then `start` from the former.
 *
 * **The `click` handler is a special case:** it will be called only on top-most parent, but its
 * handler may decide to delegate it further by calling child's handler passed as an additional
 * argument
 */
export class WidgetEditHandler {
  private interaction: WidgetEditInteraction
  /** This, or one's child interaction which is currently active */
  private currentInteraction: WidgetEditInteraction | undefined

  constructor(
    private portId: PortId,
    innerInteraction: WidgetEditInteraction,
    private parent?: WidgetEditHandler,
    private interactionHandler: InteractionHandler = injectInteractionHandler(),
  ) {
    this.interaction = {
      cancel: () => {
        this.currentInteraction = undefined
        innerInteraction.cancel?.()
        parent?.interaction.cancel?.()
      },
      click: (event, navigator, childHandler) => {
        const innerInteractionClick = innerInteraction.click
        const thisHandler =
          innerInteractionClick ?
            () => innerInteractionClick(event, navigator, childHandler)
          : childHandler
        if (parent && parent.interaction.click)
          return parent.interaction.click(event, navigator, thisHandler)
        else return thisHandler ? thisHandler() : false
      },
      start: (portId) => {
        innerInteraction.start?.(portId)
        parent?.interaction.start?.(portId)
      },
      edit: (portId, value) => {
        innerInteraction.edit?.(portId, value)
        parent?.interaction.edit?.(portId, value)
      },
      end: (portId) => {
        this.currentInteraction = undefined
        innerInteraction.end?.(portId)
        parent?.interaction.end?.(portId)
      },
    }
  }

  static New(input: WidgetInput, myInteraction: WidgetEditInteraction) {
    return new WidgetEditHandler(input.portId, myInteraction, input.editHandler)
  }

  cancel() {
    if (this.currentInteraction) {
      this.interactionHandler.cancel(this.currentInteraction)
    }
  }

  start() {
    this.interactionHandler.setCurrent(this.interaction)
    for (
      let handler: WidgetEditHandler | undefined = this;
      handler != null;
      handler = handler.parent
    ) {
      handler.currentInteraction = this.interaction
    }
    this.interaction.start?.(this.portId)
  }

  edit(value: Ast.Owned | string) {
    this.interaction.edit?.(this.portId, value)
  }

  end() {
    if (this.currentInteraction) {
      this.interactionHandler.end(this.currentInteraction)
      this.currentInteraction.end?.(this.portId)
    }
  }

  isActive() {
    return this.currentInteraction ?
        this.interactionHandler.isActive(this.currentInteraction)
      : false
  }
}
