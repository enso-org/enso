/** @file Settings tab for editing keyboard shortcuts. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'
import CrossIcon from 'enso-assets/cross.svg'
import Plus2Icon from 'enso-assets/plus2.svg'

import * as refreshHooks from '#/hooks/refreshHooks'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'

import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import SvgMask from '#/components/SvgMask'

import * as object from '#/utilities/object'

// ====================================
// === KeyboardShortcutsSettingsTab ===
// ====================================

/** Settings tab for viewing and editing account information. */
export default function KeyboardShortcutsSettingsTab() {
  const inputBindings = inputBindingsManager.useInputBindings()
  const [, doRefresh] = refreshHooks.useRefresh()

  return (
    <div className="flex flex-col gap-2.5 w-full pr-4">
      <h3 className="font-bold text-xl h-9.5 py-0.5">Keyboard shortcuts</h3>
      <table className="rounded-rows table-fixed border-collapse">
        <thead>
          <tr className="text-left text-sm font-semibold">
            <th className="min-w-4 pl-2">{/* Icon */}</th>
            <th className="min-w-36 px-1">Name</th>
            <th className="px-1">Shortcuts</th>
            <th className="pl-1 pr-3 w-full">Description</th>
          </tr>
        </thead>
        <tbody>
          {object.unsafeEntries(inputBindings.metadata).map(kv => {
            const [action, info] = kv
            return (
              <tr key={action}>
                <td className="flex h-8 items-center rounded-l-full pl-2 bg-clip-padding">
                  <SvgMask src={info.icon ?? BlankIcon} color={info.color} className="w-4 h-4" />
                </td>
                <td className="px-1 bg-clip-padding border-transparent border-l-2 border-r-2">
                  {info.name}
                </td>
                <td className="group px-1 bg-clip-padding border-transparent border-l-2 border-r-2">
                  {info.bindings.map((binding, i) => (
                    <div key={i} className="inline-flex items-center gap-1">
                      <KeyboardShortcut shortcut={binding} />
                      <button
                        className="invisible group-hover:visible"
                        onClick={() => {
                          inputBindings.delete(action, binding)
                          doRefresh()
                        }}
                      >
                        <img src={CrossIcon} />
                      </button>
                    </div>
                  ))}
                  <button
                    className="align-middle ml-2 first:ml-0 invisible group-hover:visible"
                    onClick={() => {
                      // FIXME: capture keyboard shortcut
                      inputBindings.add(action, '')
                      doRefresh()
                    }}
                  >
                    <img className="w-4.5 h-4.5" src={Plus2Icon} />
                  </button>
                </td>
                <td className="rounded-r-full pl-1 pr-3 bg-clip-padding border-transparent border-l-2 border-r-2">
                  {info.description}
                </td>
              </tr>
            )
          })}
        </tbody>
      </table>
    </div>
  )
}
