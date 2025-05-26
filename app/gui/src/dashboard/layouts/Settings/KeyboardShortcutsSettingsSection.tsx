/** @file Settings tab for viewing and editing keyboard shortcuts. */
import * as React from 'react'

import BlankIcon from '#/assets/blank.svg'
import CrossIcon from '#/assets/cross.svg'
import Plus2Icon from '#/assets/plus2.svg'
import ReloadIcon from '#/assets/reload.svg'
import { Button, ButtonGroup, DialogTrigger } from '#/components/AriaComponents'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import { Icon } from '#/components/Icon'
import { ACTION_TO_TEXT_ID } from '#/components/MenuEntry'
import { Scroller } from '#/components/Scroller'
import type { DashboardBindingKey } from '#/configurations/inputBindings'
import { useRefresh } from '#/hooks/refreshHooks'
import CaptureKeyboardShortcutModal from '#/modals/CaptureKeyboardShortcutModal'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useInputBindings } from '#/providers/InputBindingsProvider'
import { unsafeEntries } from '#/utilities/object'
import { useText } from '$/providers/react'

/** Settings tab for viewing and editing keyboard shortcuts. */
export default function KeyboardShortcutsSettingsSection() {
  const [refresh, doRefresh] = useRefresh()
  const inputBindings = useInputBindings()
  const { getText } = useText()
  const allShortcuts = React.useMemo(() => {
    // This is REQUIRED, in order to avoid disabling the `react-hooks/exhaustive-deps` lint.
    // eslint-disable-next-line @typescript-eslint/no-unused-expressions
    refresh
    return new Set(Object.values(inputBindings.metadata).flatMap((value) => value.bindings))
  }, [inputBindings.metadata, refresh])
  const visibleBindings = React.useMemo(
    () => unsafeEntries(inputBindings.metadata).filter((kv) => kv[1].rebindable !== false),
    [inputBindings.metadata],
  )

  return (
    <>
      <ButtonGroup className="grow-0">
        <DialogTrigger>
          <Button size="medium" variant="outline">
            {getText('resetAll')}
          </Button>
          <ConfirmDeleteModal
            actionText={getText('resetAllKeyboardShortcuts')}
            actionButtonLabel={getText('resetAll')}
            onConfirm={async () => {
              for (const k in inputBindings.metadata) {
                // eslint-disable-next-line no-restricted-syntax
                inputBindings.reset(k as DashboardBindingKey)
              }
              doRefresh()
              await Promise.resolve()
            }}
          />
        </DialogTrigger>
      </ButtonGroup>
      <Scroller
        scrollbar
        orientation="vertical"
        className="mb-[40px] min-h-0 flex-1"
        shadowStartClassName="top-8"
      >
        <table className="table-fixed border-collapse rounded-rows">
          <thead className="sticky top-0 z-1 bg-dashboard">
            <tr className="h-row text-left text-sm font-semibold">
              <th className="min-w-8 pl-cell-x pr-1.5">{/* Icon */}</th>
              <th className="min-w-36 px-cell-x">{getText('name')}</th>
              <th className="px-cell-x">{getText('shortcuts')}</th>
              <th className="w-full min-w-64 px-cell-x">{getText('description')}</th>
            </tr>
          </thead>
          <tbody>
            {visibleBindings.map((kv) => {
              const [action, info] = kv
              const name = getText(ACTION_TO_TEXT_ID[action])

              return (
                <tr key={action} className="rounded-rows-child">
                  <td
                    className="flex h-row items-center rounded-l-full bg-clip-padding pl-cell-x pr-1.5"
                    style={{ color: info.color }}
                  >
                    <Icon icon={info.icon ?? BlankIcon} className="size-4" />
                  </td>
                  <td className="border-l-2 border-r-2 border-transparent bg-clip-padding px-cell-x">
                    {name}
                  </td>
                  <td className="group min-w-max border-l-2 border-r-2 border-transparent bg-clip-padding px-cell-x">
                    <div className="gap-buttons flex items-center pr-4">
                      {info.bindings.map((binding, j) => (
                        <div key={j} className="inline-flex shrink-0 items-center gap-1">
                          <KeyboardShortcut
                            shortcut={binding}
                            className="rounded-lg border-0.5 border-primary/10 px-1"
                          />
                          <Button
                            variant="icon"
                            size="medium"
                            aria-label={getText('removeShortcut')}
                            tooltipPlacement="top left"
                            icon={CrossIcon}
                            showIconOnHover
                            onPress={() => {
                              inputBindings.delete(action, binding)
                              doRefresh()
                            }}
                          />
                        </div>
                      ))}
                      <div className="grow" />
                      <div className="flex shrink-0 items-center gap-1">
                        <DialogTrigger>
                          <Button
                            variant="icon"
                            size="medium"
                            aria-label={getText('addShortcut')}
                            tooltipPlacement="top left"
                            icon={Plus2Icon}
                            showIconOnHover
                          />
                          <CaptureKeyboardShortcutModal
                            description={`'${name}'`}
                            existingShortcuts={allShortcuts}
                            onSubmit={(shortcut) => {
                              inputBindings.add(action, shortcut)
                              doRefresh()
                            }}
                          />
                        </DialogTrigger>
                        <Button
                          variant="icon"
                          size="medium"
                          aria-label={getText('resetShortcut')}
                          tooltipPlacement="top left"
                          icon={ReloadIcon}
                          showIconOnHover
                          onPress={() => {
                            inputBindings.reset(action)
                            doRefresh()
                          }}
                        />
                      </div>
                    </div>
                  </td>
                  <td className="cell-x rounded-r-full border-l-2 border-r-2 border-transparent bg-clip-padding">
                    {info.description}
                  </td>
                </tr>
              )
            })}
          </tbody>
        </table>
      </Scroller>
    </>
  )
}
