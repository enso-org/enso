import { createContextStore } from '@/providers'
import { setIfUndefined } from 'lib0/map.js'
import { computed, onScopeDispose, reactive, toValue } from 'vue'
import type { ActionHandler, DisplayableActionName } from './action'

const [, useWidgetActions] = createContextStore('widget actions', () => {
  const widgetActions = reactive(new Map<string, ActionHandler[]>())
  const widgetControlledActionNames = new Set<string>()
  const warnedActionNames = new Set<string>()

  function widgetControlledAction<const N extends DisplayableActionName>(actionName: N) {
    widgetControlledActionNames.add(actionName)
    const currentAction = computed(() => {
      const candidates = widgetActions.get(actionName)
      return candidates?.find((action) => toValue(action.available) ?? true)
    })
    return {
      [actionName]: {
        action: (ctx) => currentAction.value?.action(ctx),
        available: () => currentAction.value != null,
        enabled: () => toValue(currentAction.value?.enabled) ?? true,
        description: () => toValue(currentAction.value?.description),
        icon: () => toValue(currentAction.value?.icon),
        shortcut: () => toValue(currentAction.value?.shortcut),
        toggled: () => toValue(currentAction.value?.toggled) ?? false,
      } satisfies ActionHandler,
    }
  }

  function registerHandler(actionName: string, handler: ActionHandler) {
    if (!widgetControlledActionNames.has(actionName) && !warnedActionNames.has(actionName)) {
      warnedActionNames.add(actionName)
      console.warn(
        `Widget registered an action handler for '${actionName}', but it is not declared as widget controlled.`,
      )
    }
    const registry: ActionHandler[] = setIfUndefined(widgetActions, actionName, () => reactive([]))
    registry.push(handler)
    onScopeDispose(() => registry.splice(registry.lastIndexOf(handler), 1))
  }

  return { widgetControlledAction, registerHandler }
})

/**
 * Declare actions that have their definition controlled by child widgets
 * Widgets can register a handler for those actions using {@link registerWidgetActionHandlers}.
 */
export function provideWidgetControlledActions<
  const Actions extends [DisplayableActionName, ...DisplayableActionName[]],
>(actions: Actions): { [N in Actions[number]]: ActionHandler } {
  const actionsStore = useWidgetActions(() => [] as const, true)
  return Object.assign({}, ...actions.map(actionsStore.widgetControlledAction))
}

/**
 * Register a handler for node-level actions. Only allows handlers for actions declared in as widget controlled
 * at the node level. See `provideWidgetControlledActions`.
 */
export function registerWidgetActionHandlers<
  const Handlers extends Partial<Record<DisplayableActionName, ActionHandler>>,
>(handlers: Handlers) {
  const actionsStore = useWidgetActions(true)
  if (!actionsStore) return
  for (const [actionName, ActionHandler] of Object.entries(handlers)) {
    actionsStore.registerHandler(actionName, ActionHandler)
  }
}
