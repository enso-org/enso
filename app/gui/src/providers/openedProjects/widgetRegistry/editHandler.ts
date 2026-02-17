import type { WidgetInput, WidgetTypeId } from '$/providers/openedProjects/widgetRegistry'
import type { Interaction, InteractionHandler } from '@/providers/interactionHandler'
import { injectInteractionHandler } from '@/providers/interactionHandler'
import type { PortId } from '@/providers/portInfo'
import { injectWidgetTree, type CurrentEdit } from '@/providers/widgetTree'
import type { Ast } from '@/util/ast'
import { ArgumentInfoKey } from '@/util/callTree'
import {
  computed,
  markRaw,
  shallowRef,
  toValue,
  useId,
  watch,
  type ShallowRef,
  type WatchSource,
} from 'vue'
import { assertDefined } from 'ydoc-shared/util/assert'

declare const widgetInstanceIdBrand: unique symbol
export type WidgetInstanceId = string & { [widgetInstanceIdBrand]: never }

/** Create a new unique `WidgetInstanceId` value.  */
export function newWidgetInstanceId(): WidgetInstanceId {
  return useId() as WidgetInstanceId
}

/** TODO: Add docs */
export abstract class WidgetEditHandlerParent {
  private readonly activeChild: ShallowRef<WidgetEditHandlerParent | undefined> =
    shallowRef(undefined)
  private readonly active = computed(() => this.parent?.activeChild.value === this)
  private resumableDescendants: ResumableWidgetEdits | undefined = undefined

  protected constructor(
    private readonly parent: WidgetEditHandlerParent | undefined,
    private readonly hooks: Partial<WidgetEditHooks> = {},
  ) {
    markRaw(this)
  }

  protected onStart(origin: PortId) {
    if (this.isActive()) return
    this.hooks.start?.(origin)
    this.parent?.setActiveChild(this, origin)
  }

  private setActiveChild(child: WidgetEditHandlerParent, origin: PortId) {
    this.activeChild.value?.onEnd()
    if (!this.active.value) this.onStart(origin)
    this.activeChild.value = child
    this.resumableDescendants = undefined
  }

  private unsetActiveChild(child: WidgetEditHandlerParent, origin?: PortId | undefined) {
    if (this.activeChild.value === child) {
      this.activeChild.value = undefined
      if (this.hooks.childEnded) this.hooks.childEnded(origin)
      else this.onEnd(origin)
    }
  }

  protected onEnd(origin?: PortId | undefined) {
    this.activeChild.value?.onEnd(origin)
    this.hooks.end?.(origin)
    this.parent?.unsetActiveChild(this, origin)
  }

  protected onCancel() {
    this.activeChild.value?.onCancel()
    this.hooks.cancel?.()
    this.parent?.unsetActiveChild(this)
  }

  protected onEdit(origin: PortId, value: Ast.Owned<Ast.MutableExpression> | string): void {
    this.hooks.edit?.(origin, value)
    this.parent?.onEdit(origin, value)
  }

  /** TODO: Add docs */
  addItem(): boolean {
    return this.hooks.addItem?.() ?? this.parent?.addItem() ?? false
  }

  protected pointerdown(event: PointerEvent): boolean | void {
    if (this.hooks.pointerdown && this.hooks.pointerdown(event) !== false) return true
    else return this.activeChild.value ? this.activeChild.value.pointerdown(event) : false
  }

  /** TODO: Add docs */
  isActive() {
    return this.active.value
  }

  protected activeLeaf() {
    if (!this.isActive()) return
    // This is not an alias, because it's a loop var.
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let handler: WidgetEditHandlerParent = this
    while (handler?.activeChild.value) handler = handler.activeChild.value
    return handler
  }

  protected root(): WidgetEditHandlerRoot {
    assertDefined(this.parent)
    return this.parent.root()
  }

  protected activeAncestor(): WidgetEditHandlerParent | undefined {
    return this.isActive() ? this : this.parent?.activeAncestor()
  }

  protected suspend(widgetInstance: WidgetInstanceId) {
    if (!this.isActive() || !this.parent) return
    this.parent.unsetActiveChild(this)
    this.parent.resumableDescendants ??= new Map()
    this.parent.resumableDescendants.set(widgetInstance, this.hooks.suspend?.()?.resume)
  }

  protected tryResume(widgetInstance: WidgetInstanceId, portId: PortId) {
    const ancestor = this.activeAncestor() ?? this.root().tryResumeRoot(widgetInstance)
    if (!ancestor?.resumableDescendants?.has(widgetInstance)) return
    const resumeHook = ancestor.resumableDescendants.get(widgetInstance)
    ancestor.resumableDescendants.delete(widgetInstance)
    this.resumableDescendants = ancestor.resumableDescendants
    ancestor.resumableDescendants = undefined
    this.onStart(portId)
    resumeHook?.()
  }

  protected tryTakeResumableDescendants(
    other: WidgetEditHandlerParent,
    widgetInstance: WidgetInstanceId,
  ) {
    if (!other.activeChild.value && other.resumableDescendants?.has(widgetInstance)) {
      const resumable = other.resumableDescendants
      other.resumableDescendants = undefined
      this.resumableDescendants = resumable
      return true
    }
    return false
  }
}

type ResumeCallback = () => void
type ResumableWidgetEdits = Map<WidgetInstanceId, ResumeCallback | undefined>

/** TODO: Add docs */
export class WidgetEditHandlerRoot extends WidgetEditHandlerParent implements Interaction {
  /** TODO: Add docs */
  constructor(
    private readonly currentEditCtx: CurrentEdit | undefined,
    private readonly interactionHandler: InteractionHandler,
  ) {
    super(undefined, {
      start: () => {
        this.interactionHandler.setCurrent(this)
        this.currentEditCtx?.setCurrentEditRoot(this)
      },
      end: () => this.interactionHandler.ended(this),
      cancel: () => this.interactionHandler.ended(this),
      childEnded: () => {},
    })
  }

  /** TODO: Add docs */
  tryResumeRoot(widgetInstance: WidgetInstanceId) {
    const current = this.interactionHandler.getCurrent()
    if (current instanceof WidgetEditHandlerRoot) {
      if (this.tryTakeResumableDescendants(current, widgetInstance)) return this
    }
  }

  /** TODO: Add docs */
  cancel() {
    this.onCancel()
  }

  /** TODO: Add docs */
  end() {
    this.onEnd()
  }

  /** TODO: Add docs */
  override pointerdown(event: PointerEvent) {
    return super.pointerdown(event)
  }

  protected override root() {
    return this
  }

  /** TODO: Add docs */
  override isActive() {
    return this.interactionHandler.isActive(this)
  }

  /** TODO: Add docs */
  currentEdit() {
    const leaf = this.activeLeaf()
    if (leaf !== this) return leaf
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
 * **The `pointerdown` handler is a special case:** it acts as a capture-mode event handler; it is called on
 * the top-most widget, and a widget may choose to delegate to its child (if any) by returning false.
 */
export class WidgetEditHandler extends WidgetEditHandlerParent {
  protected constructor(
    readonly portId: PortId,
    parent: WidgetEditHandlerParent | undefined,
    hooks: WidgetEditHooks,
    currentEditCtx: CurrentEdit | undefined,
    interactionHandler: InteractionHandler,
  ) {
    super(parent ?? new WidgetEditHandlerRoot(currentEditCtx, interactionHandler), hooks)
  }

  /** Create {@link WidgetEditHandler} from widget props. Convenience version of {@link NewFromPort}. */
  static New(
    props: { widgetTypeId: WidgetTypeId; input: WidgetInput },
    myInteraction: WidgetEditHooks,
  ): ShallowRef<WidgetEditHandler> {
    const widgetTree = injectWidgetTree(true)
    const interactionHandler = injectInteractionHandler()
    const portId = computed(() => props.input.portId)
    const parent = computed(() => props.input.editHandler)
    const stableInstanceId = newWidgetInstanceId()
    const instanceId = computed(() => {
      const argInfo = props.input[ArgumentInfoKey]
      return argInfo?.argId ?
          (`${argInfo.argId}||${props.widgetTypeId}` as WidgetInstanceId)
        : stableInstanceId
    })
    return WidgetEditHandler.NewRaw(
      instanceId,
      portId,
      parent,
      myInteraction,
      widgetTree,
      interactionHandler,
    )
  }

  /** Create {@link WidgetEditHandler} by manually providing a port and parent interaction. */
  static NewNested(
    portId: WatchSource<PortId>,
    parent: WatchSource<WidgetEditHandlerParent | undefined>,
    myInteraction: WidgetEditHooks,
  ): ShallowRef<WidgetEditHandler> {
    const widgetTree = injectWidgetTree(true)
    const interactionHandler = injectInteractionHandler()
    const instanceId = newWidgetInstanceId()
    return WidgetEditHandler.NewRaw(
      () => instanceId,
      portId,
      parent,
      myInteraction,
      widgetTree,
      interactionHandler,
    )
  }

  /** Create {@link WidgetEditHandler} by manually providing all needed inputs. Useful for testing. */
  static NewRaw(
    widgetInstanceId: WatchSource<WidgetInstanceId>,
    portId: WatchSource<PortId>,
    parent: WatchSource<WidgetEditHandlerParent | undefined>,
    myInteraction: WidgetEditHooks,
    currentEditCtx: CurrentEdit | undefined,
    interactionHandler: InteractionHandler,
  ): ShallowRef<WidgetEditHandler> {
    const currentHandler = shallowRef<WidgetEditHandler>(null!) // Ref is immediately assigned in watch below.
    watch(
      [portId, parent],
      ([newPortId, newParent]) => {
        const editHandler = new WidgetEditHandler(
          newPortId,
          newParent,
          myInteraction,
          currentEditCtx,
          interactionHandler,
        )
        currentHandler.value = editHandler
      },
      { immediate: true },
    )
    assertDefined(currentHandler.value)
    watch(
      currentHandler,
      (handler, _, onCleanup) => {
        handler.tryResume(toValue(widgetInstanceId), handler.portId)
        onCleanup(() => handler.suspend(toValue(widgetInstanceId)))
      },
      { immediate: true },
    )
    return currentHandler
  }

  /** TODO: Add docs */
  end() {
    this.onEnd(this.portId)
  }

  /** TODO: Add docs */
  cancel() {
    this.root().cancel()
  }

  /** TODO: Add docs */
  start() {
    this.onStart(this.portId)
  }

  /** Emit an event updating the widget's value. */
  edit(value: Ast.Owned<Ast.MutableExpression> | string) {
    this.onEdit(this.portId, value)
  }
}

/** Callbacks for {@link WidgetEditHandler} events */
export interface WidgetEditHooks extends Interaction {
  /**
   * Hook called when this widget enters its active state, either due to the `start` method of its own
   * {@link WidgetEditHandler} being called, or because a child is to be started.
   */
  start?(origin: PortId): void
  end?(origin?: PortId | undefined): void
  childEnded?(origin?: PortId | undefined): void
  /** Hook called when a child widget provides an updated value. */
  edit?(origin: PortId, value: Ast.Owned<Ast.MutableExpression> | string): void
  /**
   * Hook enabling a widget to provide a handler for the add-item intent of a child widget. The parent can return true
   * to indicate that creating the new item has been handled and the child should not perform its action in this case.
   */
  addItem?(): boolean
  /**
   * Hook called when the edit is aborted because the component instance is about to be unmounted due to a change in
   *  the graph.
   *
   *  In this case, if a successor is identified in the graph, the interaction will be restarted. If this hook is
   *  implemented, the returned `resume` function will be called after starting an interaction for this reason. It may
   *  use information captured at suspension time; however note that `resume` will be called in a different component
   *  instance from `suspend`.
   */
  suspend?(): { resume: () => void }
}
