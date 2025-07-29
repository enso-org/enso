/** @file Hooks for menus. */
import { actionToTextId, type MenuEntryProps } from '#/components/MenuEntry'
import type { DashboardBindingKey } from '#/configurations/inputBindings'
import { useBindingFocusScope } from '#/providers/BindingFocusScopeProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { DEFAULT_HANDLER } from '#/utilities/inputBindings'
import type { Action } from '$/providers/actions'
import { useActionsStore, useText } from '$/providers/react'
import { useEffect, useRef, useState } from 'react'
import { ref } from 'vue'

/** A hook to provide an input handler. */
export function useMenuEntries(entries: readonly (MenuEntryProps | false | null | undefined)[]) {
  const inputBindings = useInputBindings()
  const bindingFocusScope = useBindingFocusScope()
  const { getText } = useText()
  const { bindGlobalActions } = useActionsStore()
  const entriesByActionRef = useRef<Partial<Record<DashboardBindingKey, MenuEntryProps>>>({})
  const [actionsRef] = useState(() => ref<Action[]>([]))

  useEffect(() => {
    for (const entry of entries) {
      if (entry == null || entry === false) continue
      entriesByActionRef.current[entry.action] = entry
    }
  })

  useEffect(() => {
    actionsRef.value = entries.flatMap((entry) => {
      if (entry == null || entry === false || entry.isDisabled === true) return []
      return [
        {
          name: getText(actionToTextId(entry.action)),
          doAction: entry.doAction,
        },
      ]
    })
  }, [actionsRef, bindGlobalActions, entries, getText])

  useEffect(() => bindGlobalActions(actionsRef), [actionsRef, bindGlobalActions])

  useEffect(
    () =>
      inputBindings.attach(bindingFocusScope.current ?? document.body, 'keydown', {
        [DEFAULT_HANDLER]: (_event, matchingBindings) => {
          for (const binding of matchingBindings) {
            const entry = entriesByActionRef.current[binding]
            if (!entry || entry.isDisabled === true) {
              continue
            }
            entry.doAction()
            return
          }
          // If no handlers matched this binding, do not consider it as handled.
          return false
        },
      }),
    [bindingFocusScope, inputBindings],
  )

  return entries
}

/** A constrained identity function to more easily define a single list of menu entry. */
export function defineMenuEntry(entry: MenuEntryProps | false | null | undefined) {
  return entry
}

/**
 * A constrained identity function to more easily define a list of menu entries.
 * Prefer {@link useMenuEntries} which has the same signature if the menu entries are being
 * defined inline and used immediately.
 */
export function defineMenuEntries(entries: readonly (MenuEntryProps | false | null | undefined)[]) {
  return entries
}
