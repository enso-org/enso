/** @file Utilities for setting up menu items on Mac OS. */

import * as electron from 'electron'
import type { MenuItem } from 'enso-gui/src/project-view/util/menuItems'
import * as ipc from './ipc'

/** Make new menu item from scratch, with the given label, action, and accelerator (shortcut). */
export function makeMenuItem(
  window: electron.BrowserWindow,
  label: string,
  action: MenuItem,
  accelerator?: string,
) {
  return new electron.MenuItem({
    label,
    click: () => window.webContents.send(ipc.Channel.handleMenuItem, action),
    ...(accelerator != null ? { accelerator } : {}),
  })
}

/** Make new menu item by inheriting properties from the given item. */
export function inheritMenuItem(
  item: electron.MenuItem,
  newLabel?: string,
  newSubmenu?: electron.MenuItem[],
) {
  // `click` is a property that is intentionally removed from this
  // destructured object, in order to satisfy TypeScript.
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const { click, ...passthrough } = item
  return new electron.MenuItem({
    ...passthrough,
    ...(newLabel != null ? { label: newLabel } : {}),
    ...(newSubmenu != null ? { submenu: electron.Menu.buildFromTemplate(newSubmenu) } : {}),
  })
}

/** A predicate for choosing menu items. */
export type MenuItemFilter = (item: electron.MenuItem) => boolean

/** Create a filter that matches menu items with the given role. */
export function filterByRole(role: string): MenuItemFilter {
  // Comparing roles case-insensitively as a workaround for
  // https://github.com/electron/electron/issues/46128.
  return (item) => item.role != null && item.role.toLowerCase() === role.toLowerCase()
}

/** A replacement configuration for a menu item. Can be used to update, replace, or remove menu items with arbitrary nesting. */
export type MenuItemReplacement = {
  filter: MenuItemFilter[]
  replacement: (original: electron.MenuItem) => electron.MenuItem | undefined
}

/** Replace menu items with the given replacements, recursively descending into submenus. */
export function replaceMenuItems(
  items: electron.MenuItem[],
  replacements: MenuItemReplacement[],
  path: electron.MenuItem[] = [],
): electron.Menu {
  const newMenu = electron.Menu.buildFromTemplate(
    items.flatMap((item) => {
      const currentPath = [...path, item]
      const replacement = replacements.find(({ filter }) => {
        for (let i = 0; i < filter.length; i++) {
          const pathSegment = currentPath[i]
          if (pathSegment == null || !filter[i]?.(pathSegment)) return false
        }
        return true
      })
      if (replacement != null) {
        const newItem = replacement.replacement(item)
        return newItem != null ? [newItem] : []
      } else {
        const submenu = item.submenu
        if (submenu != null) {
          const newItems = replaceMenuItems(submenu.items, replacements, currentPath)
          return [
            {
              ...item,
              submenu: newItems,
            },
          ]
        } else {
          return [item]
        }
      }
    }),
  )
  return newMenu
}
