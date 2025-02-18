import { graphBindings, nodeEditBindings } from '@/bindings'
import { createContextStore } from '@/providers'
import { assert } from '@/util/assert'
import { Icon } from '@/util/iconMetadata/iconName'
import { ToValue } from '@/util/reactivity'
import { BindingInfo } from '@/util/shortcuts'
import { Ref } from 'vue'

export interface Action {
  action?: () => void
  icon: Icon
  shortcut?: BindingInfo
  testid?: string
  description: ToValue<string>
  hidden?: ToValue<boolean>
  disabled?: ToValue<boolean>
  toggled?: Ref<boolean>
}

const actions = {
  'components.collapse': {
    icon: 'group',
    description: 'Group Selected Components',
    shortcut: graphBindings.bindings.collapse,
  } as Action,
  'components.copy': {
    icon: 'copy2',
    description: 'Copy Components',
    shortcut: graphBindings.bindings.copyNode,
  } as Action,
  'components.deleteSelected': {
    icon: 'trash2',
    description: 'Delete Selected Components',
    shortcut: graphBindings.bindings.deleteSelected,
    testid: 'removeNode',
  } as Action,
  'components.pickColorMulti': {
    icon: 'paint_palette',
    description: 'Color Selected Components',
  } as Action,
  'component.enterNode': {
    icon: 'open',
    description: 'Open Grouped Components',
    testid: 'enter-node-button',
  } as Action,
  'component.startEditing': {
    icon: 'edit',
    description: 'Code Edit',
    shortcut: nodeEditBindings.bindings.edit,
    testid: 'edit-button',
  } as Action,
  'component.editingComment': {
    icon: 'comment',
    description: 'Add Comment',
  } as Action,
  'component.createNewNode': {
    icon: 'add',
    description: 'Add New Component',
    shortcut: graphBindings.bindings.openComponentBrowser,
  } as Action,
  'component.toggleDocPanel': {
    icon: 'help',
    description: 'Help',
  } as Action,
  'component.toggleVisualization': {
    icon: 'eye',
    description: 'Show/Hide visualization',
    shortcut: graphBindings.bindings.toggleVisualization,
  } as Action,
  'component.recompute': {
    icon: 'workflow_play',
    description: 'Write',
    testid: 'recompute',
  } as Action,
  'component.pickColor': {
    icon: 'paint_palette',
    description: 'Color Component',
  } as Action,
}

export type Actions = typeof actions

const [provideActions, injectActions] = createContextStore('Actions', (a: typeof actions) => a)

export function initializeActions() {
  provideActions(actions)
}

export function registerHandlers<
  Handlers extends { [K in keyof Actions]?: Partial<Action> } & {
    [K in Exclude<string, keyof Actions>]: never
  },
>(handlers: Handlers): Actions & Handlers {
  const actions = injectActions()
  const newActions: Actions = { ...actions }

  function isKey(k: PropertyKey): k is keyof Actions {
    return k in actions
  }

  for (const action in handlers) {
    assert(isKey(action))
    newActions[action] = {
      ...newActions[action],
      ...handlers[action],
    }
  }
  provideActions(newActions)
  return newActions as Actions & Handlers
}

export { injectActions }
