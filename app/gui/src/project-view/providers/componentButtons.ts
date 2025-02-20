import { createContextStore } from '@/providers'
import {
  reactiveButton,
  type ActionOrStateRequired,
  type Button,
  type ButtonBehavior,
  type Stateful,
  type StatefulInput,
} from '@/util/button'
import { computed, proxyRefs, type Ref, type UnwrapRef } from 'vue'

type Actions =
  | 'enterNode'
  | 'startEditing'
  | 'editingComment'
  | 'createNewNode'
  | 'toggleDocPanel'
  | 'toggleVisualization'
  | 'recompute'
  | 'pickColor'

type ActionsWithVoidActionData = Exclude<Actions, 'pickColor'>
type StatefulActions = 'toggleVisualization' | 'pickColor' | 'editingComment'

type PickColorDataInput = {
  currentColor: Ref<string | undefined>
  matchableColors: Readonly<Ref<ReadonlySet<string>>>
}
type PickColorData = UnwrapRef<PickColorDataInput>

export type ComponentButtons = Record<ActionsWithVoidActionData, Button<void>> &
  Record<'pickColor', Button<PickColorData>> &
  Record<StatefulActions, Stateful>

/**
 * Given the {@link ButtonBehavior} for each single-component button and some context, adds the UI information and
 * constructs a collection of {@link Button}s.
 */
function useComponentButtons(
  {
    graphBindings,
    nodeEditBindings,
    onBeforeAction,
  }: {
    graphBindings: Record<'openComponentBrowser' | 'toggleVisualization', { humanReadable: string }>
    nodeEditBindings: Record<'edit', { humanReadable: string }>
    onBeforeAction: () => void
  },
  buttons: Record<Actions, ButtonBehavior & ActionOrStateRequired> &
    Record<StatefulActions, StatefulInput> & {
      pickColor: { actionData: PickColorDataInput }
    },
): ComponentButtons {
  function withHooks<T extends { action?: (() => void) | undefined }>(value: T): T {
    return {
      ...value,
      action:
        value.action ?
          () => {
            onBeforeAction()
            value.action?.()
          }
        : onBeforeAction,
    }
  }
  return {
    enterNode: reactiveButton({
      ...withHooks(buttons.enterNode),
      icon: 'open',
      description: 'Open Grouped Components',
      testid: 'enter-node-button',
    }),
    startEditing: reactiveButton({
      ...withHooks(buttons.startEditing),
      icon: 'edit',
      description: 'Code Edit',
      shortcut: nodeEditBindings.edit,
      testid: 'edit-button',
    }),
    editingComment: reactiveButton({
      ...withHooks(buttons.editingComment),
      icon: 'comment',
      description: 'Add Comment',
    }),
    createNewNode: reactiveButton({
      ...withHooks(buttons.createNewNode),
      icon: 'add',
      description: 'Add New Component',
      shortcut: graphBindings.openComponentBrowser,
    }),
    toggleDocPanel: reactiveButton({
      ...withHooks(buttons.toggleDocPanel),
      icon: 'help',
      description: 'Help',
    }),
    toggleVisualization: reactiveButton({
      ...withHooks(buttons.toggleVisualization),
      icon: 'eye',
      description: computed(() =>
        buttons.toggleVisualization.state.value ? 'Hide Visualization' : 'Show Visualization',
      ),
      shortcut: graphBindings.toggleVisualization,
    }),
    recompute: reactiveButton({
      ...withHooks(buttons.recompute),
      icon: 'workflow_play',
      description: 'Write',
      testid: 'recompute',
    }),
    pickColor: reactiveButton({
      ...withHooks(buttons.pickColor),
      icon: 'paint_palette',
      description: 'Color Component',
      actionData: proxyRefs(buttons.pickColor.actionData),
    }),
  }
}

export const [provideComponentButtons, injectComponentButtons] = createContextStore(
  'Component buttons',
  useComponentButtons,
)
