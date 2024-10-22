/** @file */
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import type { PropsWithChildren } from 'react'
import { createContext, useContext, useMemo, useState } from 'react'
import type { StoreApi } from 'zustand'
import { createStore } from 'zustand'
import type { TSchema, UseFormRegisterReturn } from '../Form'

/** Context for the checkbox. */
interface CheckboxContextType {
  readonly store: StoreApi<CheckGroupPropsState>
  readonly addSelected: (selected: string) => void
  readonly removeSelected: (selected: string) => void
  readonly toggleSelected: (selected: string) => void
}

const CheckboxContext = createContext<CheckboxContextType>({
  store: createStore<CheckBoxGroupPropsStateOutsideGroup>(() => ({ insideGroup: false })),
  addSelected: () => {},
  removeSelected: () => {},
  toggleSelected: () => {},
})

/** Gets the context for the checkbox. */
export function useCheckboxContext() {
  return useContext(CheckboxContext)
}

/**
 * Gets the store for the checkbox group.
 * Returns store no matter if the checkbox is inside a group or not.
 * If the checkbox is not inside a group, the `insideGroup` property will be `false`.
 */
export function useCheckboxGroupState() {
  const { store } = useCheckboxContext()
  return store
}

/**
 * State for a checkbox group.
 * If `insideGroup` is `true`, then the checkbox is inside a group, and `selected` and `setSelected` will be defined.
 * If `insideGroup` is `false`, then the checkbox is not inside a group, and `selected` and `setSelected` will be `undefined`.
 * that means that the checkbox should be controlled manually.
 */
type CheckGroupPropsState = CheckBoxGroupPropsStateInsideGroup | CheckBoxGroupPropsStateOutsideGroup

/** Checkbox group state when the checkbox is inside a group. */
interface CheckBoxGroupPropsStateInsideGroup {
  readonly insideGroup: true
  readonly selected: Set<string>
  readonly name: string
  readonly field: UseFormRegisterReturn<TSchema>
}

/** Checkbox group state when the checkbox is not inside a group. */
interface CheckBoxGroupPropsStateOutsideGroup {
  readonly insideGroup: false
}

/** Props for {@link CheckboxGroupProvider}. */
export interface CheckboxGroupProviderProps extends PropsWithChildren {
  readonly name: string
  readonly onChange: (selected: string[]) => void
  readonly field: UseFormRegisterReturn<TSchema>
  readonly defaultValue?: string[] | undefined
}

/** Checkbox group provider used to manage the state of a group of checkboxes. */
export function CheckboxGroupProvider(props: CheckboxGroupProviderProps) {
  const { children, onChange, name, field, defaultValue = [] } = props

  const [store] = useState(() =>
    createStore<CheckBoxGroupPropsStateInsideGroup>(() => ({
      name,
      field,
      insideGroup: true,
      selected: new Set(defaultValue),
    })),
  )

  const onChangeStableCallback = useEventCallback(onChange)

  const addSelected = useEventCallback((selected: string) => {
    store.setState((state) => {
      if (state.selected.has(selected)) {
        return state
      } else {
        const nextSelected = new Set(state.selected)
        nextSelected.add(selected)

        onChangeStableCallback(Array.from(nextSelected))

        return { selected: nextSelected }
      }
    })
  })

  const removeSelected = useEventCallback((selected: string) => {
    store.setState((state) => {
      if (!state.selected.has(selected)) {
        return state
      } else {
        const nextSelected = new Set(state.selected)
        nextSelected.delete(selected)

        onChangeStableCallback(Array.from(nextSelected))

        return { selected: nextSelected }
      }
    })
  })

  const toggleSelected = useEventCallback((selected: string) => {
    store.setState((state) => {
      const nextSelected = new Set(state.selected)
      if (nextSelected.has(selected)) {
        nextSelected.delete(selected)
      } else {
        nextSelected.add(selected)
      }

      onChangeStableCallback(Array.from(nextSelected))

      return { selected: nextSelected }
    })
  })

  return (
    <CheckboxContext.Provider
      value={useMemo(
        () => ({ store, addSelected, removeSelected, toggleSelected }),
        [addSelected, removeSelected, store, toggleSelected],
      )}
    >
      {children}
    </CheckboxContext.Provider>
  )
}
