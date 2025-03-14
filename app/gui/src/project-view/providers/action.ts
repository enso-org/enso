import {
  codeEditorBindings,
  componentBrowserBindings,
  documentationEditorBindings,
  graphBindings,
  nodeEditBindings,
  undoBindings,
} from '@/bindings'
import { createContextStore } from '@/providers'
import { assert } from '@/util/assert'
import { Icon } from '@/util/iconMetadata/iconName'
import { ToValue } from '@/util/reactivity'
import { BindingInfo } from '@/util/shortcuts'
import { ref } from 'vue'
import { ForbidExcessProps } from 'ydoc-shared/util/types'

/**
 * A definition of some action available via shortcut, button, and/or menu entry.
 */
export interface Action {
  action?: () => void
  icon: Icon
  shortcut?: BindingInfo
  testid?: string
  description: ToValue<string>
  hidden?: ToValue<boolean>
  disabled?: ToValue<boolean>
  toggled?: ToValue<boolean>
}
export type ActionHandler = Partial<Action> & { action: () => void }

const actions = {
  'graphEditor.showHelp': {
    icon: 'help',
    description: 'Show help',
  },
  'components.collapse': {
    icon: 'group',
    description: 'Group Selected Components',
    shortcut: graphBindings.bindings.collapse,
  },
  'components.copy': {
    icon: 'copy2',
    description: 'Copy Components',
    shortcut: graphBindings.bindings.copyNode,
  },
  'components.deleteSelected': {
    icon: 'trash2',
    description: 'Delete Selected Components',
    shortcut: graphBindings.bindings.deleteSelected,
    testid: 'removeNode',
  },
  'components.pickColorMulti': {
    icon: 'paint_palette',
    description: 'Color Selected Components',
  },
  'component.enterNode': {
    icon: 'open',
    description: 'Open Grouped Components',
    testid: 'enter-node-button',
  },
  'component.startEditing': {
    icon: 'edit',
    description: 'Code Edit',
    shortcut: nodeEditBindings.bindings.edit,
    testid: 'edit-button',
  },
  'component.editingComment': {
    icon: 'comment',
    description: 'Add Comment',
  },
  'component.createNewNode': {
    icon: 'add',
    description: 'Add New Component',
    shortcut: graphBindings.bindings.openComponentBrowser,
  },
  'component.toggleDocPanel': {
    icon: 'help',
    description: 'Help',
  },
  'component.toggleVisualization': {
    icon: 'eye',
    description: 'Show/Hide visualization',
    shortcut: graphBindings.bindings.toggleVisualization,
  },
  'component.recompute': {
    icon: 'workflow_play',
    description: 'Write',
    testid: 'recompute',
  },
  'component.pickColor': {
    icon: 'paint_palette',
    description: 'Color Component',
  },
  'componentBrowser.editSuggestion': {
    icon: 'edit',
    description: 'Edit selected component',
    shortcut: componentBrowserBindings.bindings.applySuggestion,
  },
  'componentBrowser.acceptSuggestion': {
    icon: 'add_to_graph_editor',
    description: 'Accept selected component',
    shortcut: componentBrowserBindings.bindings.acceptSuggestion,
  },
  'componentBrowser.acceptInputAsCode': {
    icon: 'add_to_graph_editor',
    description: 'Accept search input as code',
    shortcut: componentBrowserBindings.bindings.acceptInput,
  },
  'componentBrowser.switchToCodeEditMode': {
    icon: 'edit',
    description: 'Swtich to Code Edit Mode',
    shortcut: componentBrowserBindings.bindings.switchToCodeEditMode,
  },
  'graph.addComponent': {
    icon: 'add',
    description: 'Add Component',
    shortcut: graphBindings.bindings.openComponentBrowser,
  },
  'graph.toggleCodeEditor': {
    description: 'Code Editor',
    icon: 'bottom_panel',
    shortcut: codeEditorBindings.bindings.toggle,
  },
  'graph.toggleDocumentationEditor': {
    icon: 'right_panel',
    description: 'Documentation Editor',
    shortcut: documentationEditorBindings.bindings.toggle,
  },
  'graph.renameProject': {
    description: 'Rename Project',
    icon: 'edit',
  },
  'graph.refreshExecution': {
    description: 'Refresh',
    icon: 'refresh',
  },
  'graph.recomputeAll': {
    description: 'Write All',
    icon: 'workflow_play',
  },
  'graph.undo': {
    description: 'Undo',
    shortcut: undoBindings.bindings.undo,
    icon: 'undo',
  },
  'graph.redo': {
    description: 'Redo',
    shortcut: undoBindings.bindings.redo,
    icon: 'redo',
  },
  'graph.fitAll': {
    description: 'Show All Components',
    icon: 'show_all',
  },
  'graph.zoomIn': {
    description: 'Increase Zoom',
    icon: 'add',
  },
  'graph.zoomOut': {
    description: 'Decrease Zoom',
    icon: 'minus',
  },
} satisfies Record<string, Action>

/**
 * A name of an action available in actions context.
 *
 * Such a name may be passed to `ActionButton`, `ActionMenu` or similar component instead of
 * {@link `Action`}, making use of handler defined in some ancestor.
 *
 * TODO[ao]: Also integrate it with shortcut management, preferably when working on
 * https://github.com/enso-org/enso/issues/12242
 */
export type ActionName = keyof typeof actions
type Actions = Record<ActionName, Action>

const [provideActions, injectActions] = createContextStore('Actions', (a: Actions) => a)

/**
 * Create action context and fill with basic action data (description, default shortcut etc.).
 *
 * Every panel may modify the data providing action handlers using {@link registerHandlers} method.
 */
export function initializeActions() {
  provideActions(actions)
}

/**
 * Register action handlers for this component's subcontext.
 *
 * Using this function, a component may define handlers for named actions, which in turn may be
 * used in subcomponent's buttons and menus. For example, `GraphEditor` defined handler for
 * `component.deleteSelected`, and some inner component (like `GraphNode`) may put this action in
 * its context menu.
 *
 * A component may call `registerHandlers` only once, in its setup function.
 *
 * **Note** The handlers will be visible for subcomponents injecting Actions, _not_ the current
 * component - use the return value instead.
 * @param handlers usually includes `action` or `toggled`, but may actually update any Action field
 *  (to make some context-dependent description like, if only one node is selected or many).
 * @returns All actions, with applied `handlers`.
 */
export function registerHandlers<Handlers extends Partial<Record<keyof Actions, ActionHandler>>>(
  handlers: ForbidExcessProps<Handlers, Actions>,
): Actions & Handlers {
  const actions = injectActions()
  const newActions: Actions = { ...actions }

  function isKey(k: PropertyKey): k is keyof Actions {
    return k in actions
  }

  for (const action in handlers) {
    assert(isKey(action), `${action} is not a valid Action name`)
    newActions[action] = {
      ...newActions[action],
      ...handlers[action],
    }
  }
  provideActions(newActions)
  return newActions as Actions & Handlers
}

/** A helper function for making ActionHandler toggling a boolean ref. */
export function toggledAction(toggleState = ref(false)) {
  return {
    action: () => {
      toggleState.value = !toggleState.value
    },
    toggled: toggleState,
  }
}

/**
 * Potentially resolve an action by name from context. Raises an assertion if such action is not found.
 */
export function resolveAction(actionOrName: Action | ActionName): Action {
  if (typeof actionOrName === 'string') {
    const actions = injectActions()
    assert(actions != null, 'Trying to reference an action by name, but actions not injected.')
    return actions[actionOrName]
  } else {
    return actionOrName
  }
}

export { injectActions }
