/** @file Settings tab for viewing and editing keyboard shortcuts. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'
import CrossIcon from 'enso-assets/cross.svg'
import Plus2Icon from 'enso-assets/plus2.svg'
import ReloadInCircleIcon from 'enso-assets/reload_in_circle.svg'

import type * as refreshHooks from '#/hooks/refreshHooks'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'

import KeyboardShortcutsSettingsTabBar from '#/layouts/Settings/KeyboardShortcutsSettingsTabBar'

import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/styled/UnstyledButton'
import SvgMask from '#/components/SvgMask'

import CaptureKeyboardShortcutModal from '#/modals/CaptureKeyboardShortcutModal'

import * as object from '#/utilities/object'

// ==============================
// === KeyboardShortcutsTable ===
// ==============================

/** Props for a {@link KeyboardShortcutsSettingsTabBar}. */
export interface KeyboardShortcutsTableProps {
  readonly refresh: refreshHooks.RefreshState
  readonly doRefresh: () => void
}

/** Settings tab for viewing and editing keyboard shortcuts. */
export default function KeyboardShortcutsTable(props: KeyboardShortcutsTableProps) {
  const { refresh, doRefresh } = props
  const inputBindings = inputBindingsManager.useInputBindings()
  const { setModal } = modalProvider.useSetModal()
  const rootRef = React.useRef<HTMLDivElement | null>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)
  const allShortcuts = React.useMemo(() => {
    // This is REQUIRED, in order to avoid disabling the `react-hooks/exhaustive-deps` lint.
    // eslint-disable-next-line @typescript-eslint/no-unused-expressions
    refresh
    return new Set(Object.values(inputBindings.metadata).flatMap(value => value.bindings))
  }, [inputBindings.metadata, refresh])
  const visibleBindings = React.useMemo(
    () => object.unsafeEntries(inputBindings.metadata).filter(kv => kv[1].rebindable !== false),
    [inputBindings.metadata]
  )

  // This is required to prevent the table body from overlapping the table header, because
  // the table header is transparent.
  React.useEffect(() => {
    const root = rootRef.current
    const body = bodyRef.current
    if (root != null && body != null) {
      let isClipPathUpdateQueued = false
      const updateClipPath = () => {
        isClipPathUpdateQueued = false
        body.style.clipPath = `inset(${root.scrollTop}px 0 0 0)`
      }
      const onScroll = () => {
        if (!isClipPathUpdateQueued) {
          isClipPathUpdateQueued = true
          requestAnimationFrame(updateClipPath)
        }
      }
      updateClipPath()
      root.addEventListener('scroll', onScroll)
      return () => {
        root.removeEventListener('scroll', onScroll)
      }
    } else {
      return
    }
  }, [/* should never change */ rootRef])

  return (
    // There is a horizontal scrollbar for some reason without `px-px`.
    // eslint-disable-next-line no-restricted-syntax
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div
          ref={element => {
            ref(element)
            rootRef.current = element
          }}
          className="overflow-auto px-px"
          {...innerProps}
        >
          <table className="table-fixed border-collapse rounded-rows">
            <thead className="sticky top-0">
              <tr className="h-row text-left text-sm font-semibold">
                <th className="pr-keyboard-shortcuts-icon-column-r min-w-keyboard-shortcuts-icon-column pl-cell-x">
                  {/* Icon */}
                </th>
                <th className="min-w-keyboard-shortcuts-name-column px-cell-x">Name</th>
                <th className="px-cell-x">Shortcuts</th>
                <th className="w-full px-cell-x">Description</th>
              </tr>
            </thead>
            <tbody ref={bodyRef}>
              {visibleBindings.map(kv => {
                const [action, info] = kv
                return (
                  <tr key={action}>
                    <td className="flex h-row items-center rounded-l-full bg-clip-padding pl-cell-x pr-icon-column-r">
                      <SvgMask
                        src={info.icon ?? BlankIcon}
                        color={info.color}
                        className="size-icon"
                      />
                    </td>
                    <td className="border-l-2 border-r-2 border-transparent bg-clip-padding px-cell-x">
                      {info.name}
                    </td>
                    <td className="group min-w-max border-l-2 border-r-2 border-transparent bg-clip-padding px-cell-x">
                      {/* I don't know why this padding is needed,
                       * given that this is a flex container. */}
                      {/* eslint-disable-next-line no-restricted-syntax */}
                      <div className="flex gap-buttons pr-4">
                        {info.bindings.map((binding, j) => (
                          <div
                            key={j}
                            className="inline-flex shrink-0 items-center gap-keyboard-shortcuts-button"
                          >
                            <KeyboardShortcut shortcut={binding} />
                            <UnstyledButton
                              className="flex rounded-full transition-colors hover:bg-hover-bg focus:bg-hover-bg"
                              onPress={() => {
                                inputBindings.delete(action, binding)
                                doRefresh()
                              }}
                            >
                              <SvgMask src={CrossIcon} className="size-icon" />
                            </UnstyledButton>
                          </div>
                        ))}
                        <div className="gap-keyboard-shortcuts-buttons flex shrink-0">
                          <UnstyledButton
                            className="my-auto flex rounded-full"
                            onPress={() => {
                              setModal(
                                <CaptureKeyboardShortcutModal
                                  description={`'${info.name}'`}
                                  existingShortcuts={allShortcuts}
                                  onSubmit={shortcut => {
                                    inputBindings.add(action, shortcut)
                                    doRefresh()
                                  }}
                                />
                              )
                            }}
                          >
                            <img className="size-plus-icon" src={Plus2Icon} />
                          </UnstyledButton>
                          <UnstyledButton
                            className="my-auto flex rounded-full"
                            onPress={() => {
                              inputBindings.reset(action)
                              doRefresh()
                            }}
                          >
                            <img className="size-plus-icon" src={ReloadInCircleIcon} />
                          </UnstyledButton>
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
        </div>
      )}
    </FocusArea>
  )
}
