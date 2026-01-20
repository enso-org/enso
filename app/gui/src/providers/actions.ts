import type { Icon } from '@/util/iconMetadata/iconName'
import { createGlobalState } from '@vueuse/core'
import { go } from 'fuzzysort'
import { ref, toValue, type MaybeRef, type Ref } from 'vue'

export interface Action {
  /** The name of the action. */
  name: string
  /** The category of the action. */
  category: string
  /** The function to execute when the action is triggered. */
  doAction: () => void
  shortcuts: readonly string[]
  /** The icon associated with the action, if any. */
  icon: Icon | undefined
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

type ActionsNamespaceRef = Ref<Action[] | ActionsNamespace>

function createActionsStore() {
  const actions = ref(new Set<ActionsNamespaceRef>())

  const bindGlobalActions = (newActionsNamespace: ActionsNamespaceRef) => {
    actions.value.add(newActionsNamespace)

    return () => {
      actions.value.delete(newActionsNamespace)
    }
  }

  const findActions = (query: MaybeRef<string>): readonly ActionWithHighlight[] => {
    const queryValue = toValue(query)
    const matches = go(
      queryValue,
      [...actions.value].flatMap((ref) =>
        Array.isArray(ref.value) ? ref.value : Object.values(ref.value),
      ),
      { keys: ['name', 'category'], all: true },
    )
    return matches.map((match) => ({
      ...match.obj,
      highlighted: {
        name: match[0]?.highlight('<span class="highlighted">', '</span>') || match.obj.name,
      },
    }))
  }

  return { bindGlobalActions, findActions }
}

export const useActionsStore = createGlobalState(createActionsStore)
