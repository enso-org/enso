/** @file Hooks for menus. */
import { actionToTextId, type MenuEntryProps } from '#/components/MenuEntry'
import type { DashboardBindingKey } from '#/configurations/inputBindings'
import { useBindingFocusScope } from '#/providers/BindingFocusScopeProvider'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { DEFAULT_HANDLER } from '#/utilities/inputBindings'
import type { Action } from '$/providers/actions'
import { useActionsStore, useText } from '$/providers/react'
import type { Icon } from '@/util/iconMetadata/iconName'
import { unsafeEntries } from 'enso-common/src/utilities/data/object'
import { useEffect, useRef, useState } from 'react'
import { ref } from 'vue'

/** Bind global actions given a list of handlers. */
export function useBindGlobalActions(actions: Partial<Record<DashboardBindingKey, () => void>>) {
  const inputBindings = useInputBindings()
  const { bindGlobalActions } = useActionsStore()
  const { getText } = useText()
  const actionsRef = ref<Action[]>([])

  useEffect(() => {
    actionsRef.value = unsafeEntries(actions).flatMap(([action, doAction]) => {
      if (!doAction) return []
      const metadata = inputBindings.metadata[action]
      return [
        {
          name: getText(actionToTextId(action)),
          category: getText(`${metadata.category}BindingCategory`),
          doAction,
          shortcuts: metadata.bindings,
          // eslint-disable-next-line no-restricted-syntax
          icon: metadata.icon as Icon | undefined,
        },
      ]
    })
  }, [actions, actionsRef, bindGlobalActions, getText, inputBindings.metadata])

  useEffect(() => bindGlobalActions(actionsRef), [actionsRef, bindGlobalActions])
}

/** A hook to provide an input handler. */
export function useMenuEntries(entries: readonly (MenuEntryProps | false | null | undefined)[]) {
  const inputBindings = useInputBindings()
  const bindingFocusScope = useBindingFocusScope()
  const { getText } = useText()
  const { bindGlobalActions } = useActionsStore()

  const entriesByActionRef = useRef<Map<DashboardBindingKey, MenuEntryProps>>(new Map())
  const [actionsRef] = useState(() => ref<Action[]>([]))

  useEffect(() => {
    entriesByActionRef.current.clear()
    for (const entry of entries) {
      if (entry == null || entry === false) continue
      entriesByActionRef.current.set(entry.action, entry)
    }
  })

  useEffect(() => {
    actionsRef.value = entries.flatMap((entry) => {
      if (entry == null || entry === false || entry.isDisabled === true) return []
      const metadata = inputBindings.metadata[entry.action]
      return [
        {
          name: getText(actionToTextId(entry.action)),
          category: getText(`${metadata.category}BindingCategory`),
          doAction: entry.doAction,
          shortcuts: metadata.bindings,
          // eslint-disable-next-line no-restricted-syntax
          icon: (entry.icon ?? metadata.icon) as Icon | undefined,
        },
      ]
    })
  }, [actionsRef, bindGlobalActions, entries, getText, inputBindings.metadata])

  useEffect(() => bindGlobalActions(actionsRef), [actionsRef, bindGlobalActions])

  useEffect(
    () =>
      inputBindings.attach(bindingFocusScope.current ?? document.body, 'keydown', {
        [DEFAULT_HANDLER]: (_event, matchingBindings) => {
          for (const binding of matchingBindings) {
            const entry = entriesByActionRef.current.get(binding)
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
    [bindingFocusScope, inputBindings, entries],
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
