import {
  componentBrowserBindings,
  documentationEditorFormatBindings,
  graphBindings,
  nodeEditBindings,
  panelsBindings,
  undoBindings,
} from '@/bindings'
import { createContextStore } from '@/providers'
import { type ActionContext, injectActionContext } from '@/providers/actionContext'
import { assert } from '@/util/assert'
import { Icon } from '@/util/iconMetadata/iconName'
import { type ToValue } from '@/util/reactivity'
import { BindingInfo } from '@/util/shortcuts'
import { ref } from 'vue'
import { type ForbidExcessProps } from 'ydoc-shared/util/types'

/**
 * A definition of some action available via shortcut, button, and/or menu entry.
 */
export interface Action {
  available?: ToValue<boolean>
  enabled?: ToValue<boolean>
  action?: (ctx: ActionContext | undefined) => void
  icon: Icon
  description: ToValue<string>
  shortcut?: BindingInfo
  toggled?: ToValue<boolean>
}
export type ActionHandler = Partial<Action> & { action: (ctx: ActionContext | undefined) => void }

const actions = {
  // === Graph Editor ===

  'graphEditor.showHelp': {
    icon: 'help',
    description: 'Show help',
  },

  // === Selected Components ===

  'components.collapse': {
    icon: 'group',
    description: 'Group Selected Components',
    shortcut: graphBindings.bindings['components.collapse'],
  },
  'components.copy': {
    icon: 'copy2',
    description: 'Copy Components',
    shortcut: graphBindings.bindings['components.copy'],
  },
  'components.deleteSelected': {
    icon: 'trash',
    description: 'Delete Selected Components',
    shortcut: graphBindings.bindings['components.deleteSelected'],
  },
  'components.pickColorMulti': {
    icon: 'paint_palette',
    description: 'Color Selected Components',
    shortcut: graphBindings.bindings['components.pickColorMulti'],
  },

  // === Component ===

  'component.enterNode': {
    icon: 'open',
    description: 'Open Grouped Components',
  },
  'component.startEditing': {
    icon: 'edit',
    description: 'Code Edit',
    shortcut: nodeEditBindings.bindings.edit,
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
  },
  'component.pickColor': {
    icon: 'paint_palette',
    description: 'Color Component',
  },

  // === Component Browser ===

  'componentBrowser.editSuggestion': {
    icon: 'edit',
    description: 'Edit selected component',
    shortcut: componentBrowserBindings.bindings['componentBrowser.editSuggestion'],
  },
  'componentBrowser.acceptSuggestion': {
    icon: 'add_to_graph_editor',
    description: 'Accept selected component',
    shortcut: componentBrowserBindings.bindings['componentBrowser.acceptSuggestion'],
  },
  'componentBrowser.acceptInputAsCode': {
    icon: 'add_to_graph_editor',
    description: 'Accept search input as code',
    shortcut: componentBrowserBindings.bindings['componentBrowser.acceptInputAsCode'],
  },
  'componentBrowser.switchToCodeEditMode': {
    icon: 'edit',
    description: 'Switch to Code Edit Mode',
    shortcut: componentBrowserBindings.bindings['componentBrowser.switchToCodeEditMode'],
  },

  // === Graph ===

  'graph.addComponent': {
    icon: 'add',
    description: 'Add Component',
    shortcut: graphBindings.bindings.openComponentBrowser,
  },
  'graph.toggleCodeEditor': {
    icon: 'bottom_panel',
    description: 'Code Editor',
    shortcut: panelsBindings.bindings['graph.toggleCodeEditor'],
  },
  'graph.toggleDocumentationEditor': {
    icon: 'right_panel',
    description: 'Documentation Editor',
    shortcut: panelsBindings.bindings['graph.toggleDocumentationEditor'],
  },
  'graph.renameProject': {
    icon: 'edit',
    description: 'Rename Project',
  },
  'graph.refreshExecution': {
    icon: 'refresh',
    description: 'Refresh',
  },
  'graph.recomputeAll': {
    icon: 'workflow_play',
    description: 'Write All',
  },
  'graph.undo': {
    icon: 'undo',
    description: 'Undo',
    shortcut: undoBindings.bindings['graph.undo'],
  },
  'graph.redo': {
    icon: 'redo',
    description: 'Redo',
    shortcut: undoBindings.bindings['graph.redo'],
  },
  'graph.fitAll': {
    icon: 'show_all',
    description: 'Show All Components',
    shortcut: graphBindings.bindings['graph.fitAll'],
  },
  'graph.zoomIn': {
    icon: 'add',
    description: 'Increase Zoom',
  },
  'graph.zoomOut': {
    icon: 'minus',
    description: 'Decrease Zoom',
  },
  'graph.navigateUp': {
    icon: 'navigate_up',
    description: 'Navigate Up',
    shortcut: graphBindings.bindings['graph.navigateUp'],
  },

  // === File Browser ===

  'fileBrowser.newDirectory': {
    icon: 'folder_add',
    description: 'New folder',
  },
  'fileBrowser.renameDirectory': {
    icon: 'edit',
    description: 'Rename folder',
  },
  'fileBrowser.newSecret': {
    icon: 'key_add',
    description: 'New secret',
  },
  'fileBrowser.navigateUp': {
    icon: 'navigate_up',
    description: 'Up',
  },

  // === Documentation Editor ===

  'documentationEditor.italic': {
    icon: 'italic',
    description: 'Italic',
    shortcut: documentationEditorFormatBindings.bindings['documentationEditor.italic'],
  },
  'documentationEditor.bold': {
    icon: 'bold',
    description: 'Bold',
    shortcut: documentationEditorFormatBindings.bindings['documentationEditor.bold'],
  },
  'documentationEditor.link': {
    icon: 'connector_add',
    description: 'Link',
    shortcut: documentationEditorFormatBindings.bindings['documentationEditor.link'],
  },
  'documentationEditor.code': {
    icon: 'code',
    description: 'Code',
  },
  'documentationEditor.header1': {
    icon: 'header1',
    description: 'Header 1',
    shortcut: documentationEditorFormatBindings.bindings['documentationEditor.header1'],
  },
  'documentationEditor.header2': {
    icon: 'header2',
    description: 'Header 2',
    shortcut: documentationEditorFormatBindings.bindings['documentationEditor.header2'],
  },
  'documentationEditor.header3': {
    icon: 'header3',
    description: 'Header 3',
    shortcut: documentationEditorFormatBindings.bindings['documentationEditor.header3'],
  },
  'documentationEditor.paragraph': {
    icon: 'text',
    description: 'Normal',
    shortcut: documentationEditorFormatBindings.bindings['documentationEditor.paragraph'],
  },
  'documentationEditor.list': {
    icon: 'bullet-list',
    description: 'List',
  },
  'documentationEditor.numberedList': {
    icon: 'numbered-list',
    description: 'Numbered List',
  },
  'documentationEditor.quote': {
    icon: 'quote',
    description: 'Quote',
  },
} satisfies Record<string, Action>

/**
 * A name of an action available in actions context.
 *
 * Such a name may be passed to `ActionButton`, `ActionMenu` or similar component, making use of
 * handler defined in some ancestor.
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

type ResolvedAction = Action & {
  action: () => void
  available: ToValue<boolean>
  enabled: ToValue<boolean>
}

/**
 * Potentially resolve an action by name from context. Raises an error if such action is not found.
 */
export function resolveAction(actionName: ActionName): ResolvedAction {
  const actions = injectActions()
  assert(
    actions != null,
    `Trying to reference an action by name '${actionName}', but actions not injected.`,
  )
  const action = actions[actionName]
  const ctx = injectActionContext(true)
  return {
    ...action,
    action: () => action.action?.(ctx),
    available: action.available ?? true,
    enabled: action.enabled ?? true,
  }
}

export { injectActions }
