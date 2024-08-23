/** @file Settings tab for viewing and editing keyboard shortcuts. */
import * as React from 'react'

import BlankIcon from '#/assets/blank.svg'
import CrossIcon from '#/assets/cross.svg'
import Plus2Icon from '#/assets/plus2.svg'
import ReloadIcon from '#/assets/reload.svg'

import type * as inputBindings from '#/configurations/inputBindings'

import * as refreshHooks from '#/hooks/refreshHooks'
import * as scrollHooks from '#/hooks/scrollHooks'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import FocusArea from '#/components/styled/FocusArea'
import SvgMask from '#/components/SvgMask'

import CaptureKeyboardShortcutModal from '#/modals/CaptureKeyboardShortcutModal'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import * as object from '#/utilities/object'

// ========================================
// === KeyboardShortcutsSettingsSection ===
// ========================================

/** Settings tab for viewing and editing keyboard shortcuts. */
export default function KeyboardShortcutsSettingsSection() {
  const [refresh, doRefresh] = refreshHooks.useRefresh()
  const inputBindings = inputBindingsManager.useInputBindings()
  const { setModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const rootRef = React.useRef<HTMLDivElement>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)
  const allShortcuts = React.useMemo(() => {
    // This is REQUIRED, in order to avoid disabling the `react-hooks/exhaustive-deps` lint.
    // eslint-disable-next-line @typescript-eslint/no-unused-expressions
    refresh
    return new Set(Object.values(inputBindings.metadata).flatMap((value) => value.bindings))
  }, [inputBindings.metadata, refresh])
  const visibleBindings = React.useMemo(
    () => object.unsafeEntries(inputBindings.metadata).filter((kv) => kv[1].rebindable !== false),
    [inputBindings.metadata],
  )

  const { onScroll } = scrollHooks.useStickyTableHeaderOnScroll(rootRef, bodyRef)

  return (
    <>
      <ariaComponents.ButtonGroup>
        <ariaComponents.DialogTrigger>
          <ariaComponents.Button size="medium" variant="bar">
            {getText('resetAll')}
          </ariaComponents.Button>
          <ConfirmDeleteModal
            actionText={getText('resetAllKeyboardShortcuts')}
            actionButtonLabel={getText('resetAll')}
            doDelete={() => {
              for (const k in inputBindings.metadata) {
                // eslint-disable-next-line no-restricted-syntax
                inputBindings.reset(k as inputBindings.DashboardBindingKey)
              }
              doRefresh()
            }}
          />
        </ariaComponents.DialogTrigger>
      </ariaComponents.ButtonGroup>
      <FocusArea direction="vertical" focusChildClass="focus-default" focusDefaultClass="">
        {(innerProps) => (
          <div
            {...aria.mergeProps<JSX.IntrinsicElements['div']>()(innerProps, {
              ref: rootRef,
              // There is a horizontal scrollbar for some reason without `px-px`.
              // eslint-disable-next-line no-restricted-syntax
              className: 'overflow-auto px-px',
              onScroll,
            })}
          >
            <table className="table-fixed border-collapse rounded-rows">
              <thead className="sticky top-0">
                <tr className="h-row text-left text-sm font-semibold">
                  <th className="pr-keyboard-shortcuts-icon-column-r min-w-keyboard-shortcuts-icon-column pl-cell-x">
                    {/* Icon */}
                  </th>
                  <th className="min-w-keyboard-shortcuts-name-column px-cell-x">
                    {getText('name')}
                  </th>
                  <th className="px-cell-x">{getText('shortcuts')}</th>
                  <th className="w-full min-w-keyboard-shortcuts-description-column px-cell-x">
                    {getText('description')}
                  </th>
                </tr>
              </thead>
              <tbody ref={bodyRef}>
                {visibleBindings.map((kv) => {
                  const [action, info] = kv
                  return (
                    <tr key={action}>
                      <td className="flex h-row items-center rounded-l-full bg-clip-padding pl-cell-x pr-icon-column-r">
                        <SvgMask
                          src={info.icon ?? BlankIcon}
                          color={info.color}
                          className="size-4"
                        />
                      </td>
                      <td className="border-l-2 border-r-2 border-transparent bg-clip-padding px-cell-x">
                        {info.name}
                      </td>
                      <td className="group min-w-max border-l-2 border-r-2 border-transparent bg-clip-padding px-cell-x">
                        <FocusArea direction="horizontal">
                          {(bindingsProps) => (
                            <div {...bindingsProps}>
                              {/* I don't know why this padding is needed,
                               * given that this is a flex container. */}
                              {/* eslint-disable-next-line no-restricted-syntax */}
                              <div className="gap-buttons flex items-center pr-4">
                                {info.bindings.map((binding, j) => (
                                  <div
                                    key={j}
                                    className="inline-flex shrink-0 items-center gap-keyboard-shortcuts-button"
                                  >
                                    <KeyboardShortcut shortcut={binding} />
                                    <ariaComponents.Button
                                      variant="ghost"
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
                                <div className="gap-keyboard-shortcuts-buttons flex shrink-0 items-center">
                                  <ariaComponents.Button
                                    variant="ghost"
                                    size="medium"
                                    aria-label={getText('addShortcut')}
                                    tooltipPlacement="top left"
                                    icon={Plus2Icon}
                                    showIconOnHover
                                    onPress={() => {
                                      setModal(
                                        <CaptureKeyboardShortcutModal
                                          description={`'${info.name}'`}
                                          existingShortcuts={allShortcuts}
                                          onSubmit={(shortcut) => {
                                            inputBindings.add(action, shortcut)
                                            doRefresh()
                                          }}
                                        />,
                                      )
                                    }}
                                  />
                                  <ariaComponents.Button
                                    variant="ghost"
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
                            </div>
                          )}
                        </FocusArea>
                      </td>
                      <td className="cell-x rounded-r-full border-l-2 border-r-2 border-transparent bg-clip-padding">
                        {info.description}
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
          </div>
        )}
      </FocusArea>
    </>
  )
}
