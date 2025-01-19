import { type Icon } from '@/util/iconMetadata/iconName'
import { computed, type ComputedRef, markRaw, type MaybeRef, type Ref, unref } from 'vue'

export type ActionOrStateRequired = { action: () => void } | { state: Ref<boolean> }

export interface ButtonBehavior {
  action?: (() => void) | undefined
  state?: Ref<boolean> | undefined
  hidden?: ComputedRef<boolean> | undefined
  disabled?: ComputedRef<boolean> | undefined
}
export interface ButtonUI {
  icon: Icon
  description: ComputedRef<string> | string
  shortcut?: string | undefined
  testid?: string | undefined
}

/**
 * Defines the appearance and behavior of an action or toggleable state, that may be rendered as a button or menu entry.
 *
 * By enabling separation between where buttons are logically defined and where they are displayed by UI components,
 * this type solves two problems:
 * - Routing data and events: A `Button` can be created in the context where its behavior is most logically defined, and
 *   then made available to the UI components that render it; this avoids the need to pass the various state needed by
 *   different actions through the hierarchy of UI components.
 * - Duplication: A single `Button` can be used to render buttons or menu entries in various UI containers. This ensures
 *   consistency in appearance and behavior of different UI elements controlling the same action or state.
 */
export interface Button<T = unknown> {
  readonly action: (() => void) | undefined
  readonly hidden: boolean
  readonly disabled: boolean
  state: boolean | undefined
  readonly icon: Icon
  readonly shortcut: string | undefined
  readonly testid: string | undefined
  readonly description: string
  readonly descriptionWithShortcut: string
  readonly actionData: T
}

class ReactiveButton<T> implements Button<T> {
  private readonly toDescriptionWithShortcut: MaybeRef<string>
  constructor(
    readonly action: (() => void) | undefined,
    readonly icon: Icon,
    readonly shortcut: string | undefined,
    readonly testid: string | undefined,
    readonly actionData: T,
    private readonly toDescription: MaybeRef<string>,
    private readonly toHidden: Readonly<Ref<boolean>> | undefined,
    private readonly toDisabled: Readonly<Ref<boolean>> | undefined,
    private readonly refState: Ref<boolean> | undefined,
  ) {
    markRaw(this)
    this.toDescriptionWithShortcut =
      shortcut ? computed(() => `${unref(toDescription)} (${shortcut})`) : toDescription
  }
  get description(): string {
    return unref(this.toDescription)
  }
  get descriptionWithShortcut(): string {
    return unref(this.toDescriptionWithShortcut)
  }
  get hidden(): boolean {
    return this.toHidden ? unref(this.toHidden) : false
  }
  get disabled(): boolean {
    return this.toDisabled ? unref(this.toDisabled) : false
  }
  get state(): boolean | undefined {
    return this.refState && unref(this.refState)
  }
  set state(state: boolean) {
    if (this.refState) this.refState.value = state
  }
}

type ButtonInputs<T> = Omit<ButtonBehavior & ButtonUI, 'shortcut'> & {
  shortcut?: { humanReadable: string }
} & {
  actionData?: T
} & (T extends void ? unknown
  : {
      actionData: T
    })

export interface StatefulInput {
  state: Ref<boolean>
}
export interface Stateful {
  state: boolean
}

export function reactiveButton<T = void>(
  inputs: ButtonInputs<T> & StatefulInput,
): Button<T> & Stateful
export function reactiveButton<T = void>(inputs: ButtonInputs<T>): Button<T>
/** Creates a reactive {@link Button}. */
export function reactiveButton<T = void>(inputs: ButtonInputs<T>): Button<T> {
  return new ReactiveButton<T>(
    inputs.action,
    inputs.icon,
    inputs.shortcut?.humanReadable,
    inputs.testid,
    inputs.actionData as T,
    inputs.description,
    inputs.hidden,
    inputs.disabled,
    inputs.state,
  )
}
