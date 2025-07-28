import { createGlobalState } from '@vueuse/core'
import { go } from 'fuzzysort'

export interface Action {
  /** The name of the action. */
  name: string
  /** The function to execute when the action is triggered. */
  doAction: () => void
}

export interface ActionWithHighlight extends Action {
  /** Highlighted fields of the action. */
  highlighted: {
    /** The highlighted name of the action. */
    name: string
  }
}

/** A mapping from action names to {@link Action}s. */
export type ActionsNamespace = Record<string, Action>

/** The interface exposed by {@link createActionsStore}. */
export interface ActionsStore extends ReturnType<typeof createActionsStore> {}

function createActionsStore() {
  const actions = new Set<ActionsNamespace>()

  const bindGlobalActions = (newActions: Action[] | ActionsNamespace) => {
    const newActionsNamespace = (() => {
      if (Array.isArray(newActions)) {
        const namespace: ActionsNamespace = {}
        for (const action of newActions) {
          namespace[action.name] = action
        }
        return namespace
      }
      return newActions
    })()
    actions.add(newActionsNamespace)

    return () => {
      actions.delete(newActionsNamespace)
    }
  }

  const findActions = (query: string): readonly ActionWithHighlight[] => {
    const matches = go(
      query,
      [...actions].flatMap((namespace) => Object.values(namespace)),
      { keys: ['name'], all: true },
    )
    return matches.map((match) => ({
      ...match.obj,
      highlighted: {
        name: match[0]?.highlight('<span class="highlighted">', '</span>') ?? match.obj.name,
      },
    }))
  }

  return { bindGlobalActions, findActions }
}

export const useActionsStore = createGlobalState(createActionsStore)
