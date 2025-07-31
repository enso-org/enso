/** @file Hooks for menus. */
import type { MenuEntryProps } from '#/components/MenuEntry'
import type { DashboardBindingKey } from '#/configurations/inputBindings'
import { useBindingFocusScope } from '#/providers/BindingFocusScopeProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { DEFAULT_HANDLER } from '#/utilities/inputBindings'
import { useEffect, useRef } from 'react'

/** A hook to provide an input handler. */
export function useMenuEntries(entries: readonly (MenuEntryProps | false | null | undefined)[]) {
  const inputBindings = useInputBindings()
  const bindingFocusScope = useBindingFocusScope()
  const entriesByActionRef = useRef<Partial<Record<DashboardBindingKey, MenuEntryProps>>>({})

  useEffect(() => {
    for (const entry of entries) {
      if (entry == null || entry === false) {
        continue
      }
      entriesByActionRef.current[entry.action] = entry
    }
  })

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
