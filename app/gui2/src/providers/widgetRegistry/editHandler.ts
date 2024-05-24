import type { GraphNavigator } from '@/providers/graphNavigator'
import type { Interaction, InteractionHandler } from '@/providers/interactionHandler'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import type { WidgetInput } from '@/providers/widgetRegistry'
import { injectWidgetTree } from '@/providers/widgetTree'
import type { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import { computed, markRaw, onBeforeUnmount, ref, shallowRef, type Ref } from 'vue'

declare const brandWidgetId: unique symbol
/** Uniquely identifies a widget type. */
export type WidgetId = string & { [brandWidgetId]: true }

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
 * **The `pointerdown` handler is a special case:** it acts as a capture-mode event handler; it is called on
 * the top-most widget, and a widget may choose to delegate to its child (if any) by returning false.
 */
export class WidgetEditHandler {
  readonly active = computed(() => this.portEdit.value?.active.value ?? false)
  private readonly portEdit = shallowRef<PortEditInteraction>()
  private readonly interaction: PortEditSubinteraction

  constructor(
    private readonly widgetId: WidgetId,
    private readonly hooks: WidgetEditHooks,
    private readonly parent: WidgetEditHandler | undefined,
    private readonly portEditor: PortEditInitiatorOrResumer,
    private readonly widgetTree: {
      currentEdit: WidgetEditHandler | undefined
    } = injectWidgetTree(),
  ) {
    const noLongerActive = () => {
      if (widgetTree.currentEdit === this) {
        widgetTree.currentEdit = undefined
      }
      this.portEdit.value = undefined
    }
    this.interaction = {
      widgetId,
      cancel: () => {
        noLongerActive()
        hooks.cancel?.()
      },
      end: (origin?: WidgetId) => {
        noLongerActive()
        hooks.end?.(origin)
      },
      pointerdown: (event, navigator) => {
        if (!hooks.pointerdown) return false
        return hooks.pointerdown?.(event, navigator)
      },
    }
  }

  static New(
    widgetId: string,
    input: WidgetInput,
    myInteraction: WidgetEditHooks,
  ): WidgetEditHandler {
    const editHandler = new WidgetEditHandler(
      widgetId as WidgetId,
      myInteraction,
      input.editHandler,
      new PortEditInitiatorOrResumer(
        input.portId,
        input[ArgumentInfoKey]?.argId,
        injectInteractionHandler(),
      ),
    )
    const portEdit = editHandler.portEditor.tryResume()
    if (portEdit) {
      const resumeHook = portEdit.getResumeHook(widgetId as WidgetId)
      if (resumeHook) {
        editHandler.start()
        resumeHook()
      }
    }
    if (input.editHandler == null) {
      onBeforeUnmount(() => editHandler.portEdit.value?.suspend())
    }
    return editHandler
  }

  cancel() {
    this.portEdit.value?.cancel()
  }

  start() {
    this.onStart(this.widgetId)
    this.widgetTree.currentEdit = markRaw(this)
  }

  private onStart(origin: WidgetId) {
    this.parent?.onStart?.(origin)
    this.portEdit.value = this.portEditor.start()
    this.portEdit.value.register(this.interaction)
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
    this.portEdit.value?.end(this.widgetId)
  }

  isActive() {
    return this.active.value
  }

  addItem(): boolean {
    return (this.parent?.addItem() || this.hooks.addItem?.()) ?? false
  }
}

/** Callbacks for {@link WidgetEditHandler} events */
export interface WidgetEditHooks extends Interaction {
  /**
   * Hook called when this widget enters its active state, either due to the `start` method of its own
   * {@link WidgetEditHandler} being called, or because a child is to be started.
   */
  start?(origin: WidgetId): void
  end(origin?: WidgetId | undefined): void
  /**
   * Hook called when a child widget, or this widget itself, provides an updated value.
   */
  edit?(origin: WidgetId, value: Ast.Owned | string): void
  /**
   * Hook enabling a widget to provide a handler for the add-item intent of a child widget. The parent can return true
   * to indicate that creating the new item has been handled and the child should not perform its action in this case.
   */
  addItem?(): boolean
  /** Hook called when the edit is aborted because the component instance is about to be unmounted due to a change in
   *  the graph.
   *
   *  In this case, if a successor is identified in the graph, the interaction will be restarted. If this hook is
   *  implemented, the returned `resume` function will be called after starting an interaction for this reason. It may
   *  use information captured at suspension time; however note that `resume` will be called in a different component
   *  instance from `suspend`.
   */
  suspend?(): { resume: () => void }
}

type ResumeCallback = (() => void) | undefined
type PortEditResumeData = Map<WidgetId, ResumeCallback>

/** Top-level interaction active when a port is being edited by associated widgets.
 *
 *  `PortEditInteraction` contains sub-interactions for all active widgets, and distributes interaction events to them.
 *
 *  It manages *active state*, which can be started, stopped, suspended and resumed; this is separate from the static
 *  parent/child chain of edit-handlers on a port, which is used for events such as `add-item` that may occur
 *  independently of the active-states of widgets and follow a strict parent-child relationship.
 */
class PortEditInteraction implements Interaction {
  readonly active: Ref<boolean>
  private readonly resumable: PortEditResumeData
  private readonly interactions = new Array<PortEditSubinteraction>()

  private constructor(
    public readonly portId: PortId,
    public readonly argId: string | undefined,
    resumable: PortEditResumeData | undefined,
    active: boolean,
    private readonly interactionHandler = injectInteractionHandler(),
  ) {
    this.resumable = resumable ?? new Map()
    this.active = ref(active)
  }

  static Start(
    portId: PortId,
    argId: string | undefined,
    interactionHandler = injectInteractionHandler(),
    resumable?: Map<WidgetId, ResumeCallback> | undefined,
  ) {
    const interaction = new PortEditInteraction(portId, argId, resumable, true, interactionHandler)
    interactionHandler.setCurrent(interaction)
    return interaction
  }

  pointerdown(event: PointerEvent, navigator: GraphNavigator): boolean | void {
    for (const interaction of this.interactions) {
      if (!interaction.pointerdown) continue
      if (interaction.pointerdown(event, navigator) !== false) break
    }
    return false
  }

  cancel() {
    for (const interaction of this.interactions) interaction.cancel()
    this.shutdown()
  }

  end(origin?: WidgetId) {
    for (const interaction of this.interactions) interaction.end?.(origin)
    this.shutdown()
  }

  private shutdown() {
    this.interactions.length = 0
    this.active.value = false
    this.interactionHandler.ended(this)
  }

  register(interaction: PortEditSubinteraction) {
    this.interactions.push(interaction)
    this.resumable?.delete(interaction.widgetId)
  }

  suspend() {
    if (this.argId === undefined) return
    const suspendedWidgets = new Map(
      this.interactions.map((interaction) => [
        interaction.widgetId,
        interaction.suspend?.().resume,
      ]),
    )
    this.interactionHandler.setCurrent(new SuspendedPortEdit(this.argId, suspendedWidgets))
  }

  getResumeHook(widgetId: WidgetId) {
    return this.resumable.has(widgetId) ? this.resumable.get(widgetId) ?? (() => {}) : undefined
  }
}

/** A top-level interaction containing suspended state for {@PortEditSubinteractions} that were aborted when a
 *  component was unmounted. This state may be used (when a successor for the original port is found) or discarded (if
 *  the user starts a new top-level interaction, replacing this one). */
class SuspendedPortEdit implements Interaction {
  constructor(
    public readonly argId: string,
    public readonly resumable: PortEditResumeData,
  ) {}

  pointerdown() {
    return false
  }

  cancel() {}

  end() {}
}

/** A sub-interaction of a @{link PortEditInteraction} */
interface PortEditSubinteraction extends Interaction {
  widgetId: WidgetId

  suspend?: () => { resume: () => void }

  end(origin?: WidgetId | undefined): void
}

/** @internal Public for unit testing.
 *  Obtains a top-level interaction to edit a port (see {@link PortEditInteraction}), which may be a pre-existing
 *  ongoing interaction, or a newly-started interaction. */
export class PortEditInitiatorOrResumer {
  constructor(
    private readonly portId: PortId,
    private readonly argId: string | undefined,
    private readonly interactionHandler: InteractionHandler,
  ) {}

  start() {
    const current = this.interactionHandler.getCurrent()
    if (current instanceof PortEditInteraction && current.portId === this.portId) {
      return current
    } else {
      return this.newEdit()
    }
  }

  tryResume() {
    if (this.argId == null) return
    const current = this.interactionHandler.getCurrent()
    if (current instanceof SuspendedPortEdit && this.argId === current.argId) {
      return this.newEdit(current.resumable)
    } else {
      return
    }
  }

  private newEdit(resumable?: PortEditResumeData) {
    return PortEditInteraction.Start(this.portId, this.argId, this.interactionHandler, resumable)
  }
}
