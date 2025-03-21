/** @file Context for Checkbox. */
import {
  CheckboxContext,
  type CheckBoxGroupPropsStateInsideGroup,
} from '#/components/AriaComponents/Checkbox/constants'
import type { TSchema, UseFormRegisterReturn } from '#/components/AriaComponents/Form/types'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { createStore } from '#/utilities/zustand'
import type { PropsWithChildren } from 'react'
import { useState } from 'react'

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
    <CheckboxContext.Provider value={{ store, addSelected, removeSelected, toggleSelected }}>
      {children}
    </CheckboxContext.Provider>
  )
}

/** Props for {@link CheckboxStandaloneProvider}. */
export interface CheckboxStandaloneProviderProps extends PropsWithChildren {
  readonly name: string
  readonly onChange: (selected: boolean) => void
  readonly field: UseFormRegisterReturn<TSchema>
  readonly defaultValue?: boolean | undefined
}

/**
 * Provider for a standalone checkbox.
 * This is used when the checkbox is not inside a group.
 */
export function CheckboxStandaloneProvider(props: CheckboxStandaloneProviderProps) {
  const { children, name, field, defaultValue = false, onChange } = props

  const [store] = useState(() =>
    createStore<CheckBoxGroupPropsStateInsideGroup>(() => ({
      name,
      field,
      insideGroup: true,
      selected: defaultValue === true ? new Set([name]) : new Set(),
    })),
  )

  const onChangeStableCallback = useEventCallback(() => {
    onChange(store.getState().selected.size === 1)
  })

  const addSelected = useEventCallback((selected: string) => {
    store.setState((state) => {
      if (state.selected.has(selected)) {
        return state
      }

      const nextSelected = new Set(state.selected)
      nextSelected.add(selected)

      onChangeStableCallback()

      return { selected: nextSelected }
    })
  })

  const removeSelected = useEventCallback((selected: string) => {
    store.setState((state) => {
      if (!state.selected.has(selected)) {
        return state
      }

      const nextSelected = new Set(state.selected)
      nextSelected.delete(selected)

      onChangeStableCallback()

      return { selected: nextSelected }
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

      onChangeStableCallback()

      return { selected: nextSelected }
    })
  })

  return (
    <CheckboxContext.Provider value={{ store, addSelected, removeSelected, toggleSelected }}>
      {children}
    </CheckboxContext.Provider>
  )
}
