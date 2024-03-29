import type { GraphNavigator } from '@/providers/graphNavigator'
import type { Interaction, InteractionHandler } from '@/providers/interactionHandler'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import type { WidgetInput } from '@/providers/widgetRegistry'
import type { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { markRaw } from 'vue'
import { injectWidgetTree } from '../widgetTree'

declare const brandWidgetId: unique symbol
export type WidgetId = string & { [brandWidgetId]: true }

class PortEditInteraction implements Interaction {
  private interactions = new Array<WidgetEditInteraction>()
  private suspended: Set<WidgetId> | undefined

  constructor(
    public readonly portId: PortId,
    public readonly argId: string | undefined,
    private readonly interactionHandler = injectInteractionHandler(),
  ) {}

  static Start(
    portId: PortId,
    argId: string | undefined,
    interactionHandler = injectInteractionHandler(),
  ) {
    const current = interactionHandler.getCurrent()
    if (current instanceof PortEditInteraction && current.portId === portId) {
      return current
    } else {
      const interaction = new PortEditInteraction(portId, argId, interactionHandler)
      interactionHandler.setCurrent(interaction)
      return interaction
    }
  }

  pointerdown?(event: PointerEvent, navigator: GraphNavigator): boolean | void {
    for (const interaction of this.interactions) {
      if (!interaction.pointerdown) continue
      if (interaction.pointerdown(event, navigator) !== false) break
    }
    return false
  }

  cancel() {
    for (const interaction of this.interactions) interaction.cancel()
    this.interactions.length = 0
  }

  ///////////

  end(origin: WidgetId) {
    for (const interaction of this.interactions) interaction.end?.(origin)
    this.interactions.length = 0
  }

  isActive() {
    return this.interactionHandler.getCurrent() === this
  }

  register(interaction: WidgetEditInteraction) {
    this.interactions.push(interaction)
  }

  suspend() {
    if (!this.suspended) {
      this.suspended = new Set(...[this.interactions.map((interaction) => interaction.widgetId)])
      this.cancel()
    }
  }
}

interface Endable {
  end?(origin: WidgetId): void
}
/** An interaction used in {@link WidgetEditHandler} */
interface WidgetEditInteraction extends Interaction, Endable {
  widgetId: WidgetId
}
export interface WidgetEditHooks extends Interaction, Endable {
  start?(origin: WidgetId): void
  edit?(origin: WidgetId, value: Ast.Owned | string): void
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
 * {@link WidgetEditHooks} passed during construction.
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
 * **The `click` handler is a special case:** it acts as a capture-mode event handler; it is called on
 * the top-most widget, and a widget may choose to delegate to its child (if any) by returning false.
 */
export class WidgetEditHandler {
  private readonly interaction: WidgetEditInteraction
  private portEdit: PortEditInteraction | undefined

  constructor(
    private readonly widgetId: WidgetId,
    private readonly portId: PortId,
    private readonly argId: string | undefined,
    private readonly hooks: WidgetEditHooks,
    private readonly parent: WidgetEditHandler | undefined,
    private readonly interactionHandler: InteractionHandler,
    private readonly widgetTree: {
      currentEdit: WidgetEditHandler | undefined
    } = injectWidgetTree(),
  ) {
    const noLongerActive = () => {
      if (widgetTree.currentEdit === this) {
        widgetTree.currentEdit = undefined
      }
      this.portEdit = undefined
    }
    this.interaction = {
      widgetId,
      cancel: () => {
        noLongerActive()
        hooks.cancel?.()
      },
      end: (origin: WidgetId) => {
        noLongerActive()
        hooks.end?.(origin)
      },
      pointerdown: (event, navigator) => {
        if (!hooks.pointerdown) return false
        return hooks.pointerdown?.(event, navigator)
      },
    }
  }

  static New(widgetId: string, input: WidgetInput, myInteraction: WidgetEditHooks): WidgetEditHandler {
    const interactionHandler = injectInteractionHandler()
    const editHandler = new WidgetEditHandler(
      widgetId as WidgetId,
      input.portId,
      input[ArgumentInfoKey]?.argId,
      myInteraction,
      input.editHandler,
      interactionHandler,
    )
    return editHandler
  }

  cancel() {
    this.portEdit?.cancel()
  }

  start() {
    this.onStart(this.widgetId)
    this.widgetTree.currentEdit = markRaw(this)
  }

  private onStart(origin: WidgetId) {
    this.parent?.onStart?.(origin)
    const portEdit = PortEditInteraction.Start(this.portId, this.argId, this.interactionHandler)
    portEdit.register(this.interaction)
    this.portEdit = portEdit
    this.hooks.start?.(origin)
  }

  edit(value: Ast.Owned | string) {
    this.onEdit(this.widgetId, value)
  }

  private onEdit(origin: WidgetId, value: Ast.Owned | string) {
    this.hooks.edit?.(origin, value)
    this.parent?.onEdit?.(origin, value)
  }

  end() {
    this.portEdit?.end(this.widgetId)
  }

  isActive() {
    return this.portEdit?.isActive?.() ?? false
  }

  addItem(): boolean {
    return (this.parent?.addItem() || this.hooks.addItem?.()) ?? false
  }
}
