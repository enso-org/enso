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
export type WidgetId = string & { [brandWidgetId]: true }

type ResumeCallback = (() => void) | undefined
type PortEditResumeData = Map<WidgetId, ResumeCallback>

class SuspendedPortEdit implements Interaction {
  constructor(
    public readonly argId: string,
    public readonly resumable: PortEditResumeData,
  ) {}

  pointerdown() {
    return false
  }

  cancel() {}
}

class PortEditInteraction implements Interaction {
  readonly portId: PortId
  readonly argId: string | undefined
  readonly active: Ref<boolean>
  private readonly interactionHandler
  private readonly resumable: PortEditResumeData
  private readonly interactions = new Array<PortEditSubinteraction>()

  private constructor(
    portId: PortId,
    argId: string | undefined,
    resumable: PortEditResumeData | undefined,
    active: boolean,
    interactionHandler = injectInteractionHandler(),
  ) {
    this.portId = portId
    this.argId = argId
    this.resumable = resumable ?? new Map()
    this.active = ref(active)
    this.interactionHandler = interactionHandler
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
    this.interactions.length = 0
    this.active.value = false
    this.interactionHandler.end(this)
  }

  end(origin: WidgetId) {
    for (const interaction of this.interactions) interaction.end?.(origin)
    this.interactions.length = 0
    this.active.value = false
    this.interactionHandler.end(this)
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

interface Endable {
  end?(origin: WidgetId): void
}
/** A child interaction of a @{link PortEditInteraction} */
interface PortEditSubinteraction extends Interaction, Endable {
  widgetId: WidgetId

  suspend?: () => { resume: () => void }
}

export interface WidgetEditHooks extends Interaction, Endable {
  start?(origin: WidgetId): void
  /**
   * Hook call when a child widget, or this widget itself, provides an updated value.
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

/** @internal Public for unit testing.
 *  Obtains a top-level interaction to edit a port (see {@link PortEditInteraction}), which may be a pre-existing
 *  ongoing interaction, or a newly-started interaction. */
export class PortEditor {
  private readonly portId: PortId
  private readonly argId: string | undefined
  private readonly interactionHandler: InteractionHandler

  constructor(portId: PortId, argId: string | undefined, interactionHandler: InteractionHandler) {
    this.portId = portId
    this.argId = argId
    this.interactionHandler = interactionHandler
  }

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
  readonly active = computed(() => this.portEdit.value?.active.value ?? false)
  private readonly portEdit = shallowRef<PortEditInteraction>()
  private readonly interaction: PortEditSubinteraction
  private readonly portEditor: PortEditor
  private readonly widgetId: WidgetId
  private readonly hooks: WidgetEditHooks
  private readonly parent: WidgetEditHandler | undefined
  private readonly widgetTree: {
    currentEdit: WidgetEditHandler | undefined
  }

  constructor(
    widgetId: WidgetId,
    hooks: WidgetEditHooks,
    parent: WidgetEditHandler | undefined,
    portEditor: PortEditor,
    widgetTree: {
      currentEdit: WidgetEditHandler | undefined
    } = injectWidgetTree(),
  ) {
    this.widgetId = widgetId
    this.hooks = hooks
    this.parent = parent
    this.widgetTree = widgetTree
    this.portEditor = portEditor
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

  static New(
    widgetId: string,
    input: WidgetInput,
    myInteraction: WidgetEditHooks,
  ): WidgetEditHandler {
    const editHandler = new WidgetEditHandler(
      widgetId as WidgetId,
      myInteraction,
      input.editHandler,
      new PortEditor(input.portId, input[ArgumentInfoKey]?.argId, injectInteractionHandler()),
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
