import type { PortId } from '@/providers//portInfo'
import type { GraphNavigator } from '@/providers/graphNavigator'
import { injectInteractionHandler, type Interaction } from '@/providers/interactionHandler'
import type { WidgetInput } from '@/providers/widgetRegistry'
import type { Ast } from '@/util/ast'
import { markRaw } from 'vue'
import { injectWidgetTree } from '../widgetTree'

/** An extend {@link Interaction} used in {@link WidgetEditHandler} */
export interface WidgetEditInteraction extends Interaction {
  /** Click handler from {@link Interaction}, but receives child's click handler. See
   * {@link WidgetEditHandler} for details */
  click?(
    event: PointerEvent,
    navigator: GraphNavigator,
    childHandler?: () => boolean | void,
  ): boolean | void
  start?(origin: PortId): void
  edit?(origin: PortId, value: Ast.Owned | string): void
  end?(origin: PortId): void
  addItem?(): boolean
}

/**
 * Widget edit handler.
 *
 * This handler takes an extended interaction and allows cooperation between parent/child
 * interactions. A usage example is WidgetSelection, which wants to open when the child is edited
 * and filters entries by edited temporary value.
 *
 * Widget's edit state should be manipulated by `start`, `end` and `cancel` methods; they will set
 * proper interaction in the global {@link InteractionHandler} and call the additional callbacks in
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
  private readonly interaction: WidgetEditInteraction
  /** This, or one's child interaction which is currently active */
  private activeInteraction: WidgetEditInteraction | undefined

  constructor(
    private portId: PortId,
    innerInteraction: WidgetEditInteraction,
    private parent?: WidgetEditHandler,
    private interactionHandler = injectInteractionHandler(),
    private widgetTree: { currentEdit: WidgetEditHandler | undefined } = injectWidgetTree(),
  ) {
    const noLongerActive = () => {
      this.activeInteraction = undefined
      if (widgetTree.currentEdit === this) {
        widgetTree.currentEdit = undefined
      }
    }
    this.interaction = {
      cancel: () => {
        noLongerActive()
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
        noLongerActive()
        innerInteraction.end?.(portId)
        parent?.interaction.end?.(portId)
      },
      addItem: () => {
        return (innerInteraction.addItem?.() || parent?.interaction.addItem?.()) ?? false
      },
    }
  }

  static New(input: WidgetInput, myInteraction: WidgetEditInteraction) {
    return new WidgetEditHandler(input.portId, myInteraction, input.editHandler)
  }

  cancel() {
    if (this.activeInteraction) {
      this.interactionHandler.cancel(this.activeInteraction)
    }
  }

  start() {
    this.interactionHandler.setCurrent(this.interaction)
    this.widgetTree.currentEdit = markRaw(this)
    for (
      let handler: WidgetEditHandler | undefined = this;
      handler != null;
      handler = handler.parent
    ) {
      handler.activeInteraction = this.interaction
    }
    this.interaction.start?.(this.portId)
  }

  edit(value: Ast.Owned | string) {
    this.interaction.edit?.(this.portId, value)
  }

  end() {
    if (this.activeInteraction) {
      this.interactionHandler.end(this.activeInteraction)
      this.activeInteraction.end?.(this.portId)
    }
  }

  isActive() {
    return this.activeInteraction ? this.interactionHandler.isActive(this.activeInteraction) : false
  }

  addItem() {
    return this.interaction.addItem?.()
  }
}
