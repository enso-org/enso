/** @file Settings tab for viewing and editing keyboard shortcuts. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'
import CrossIcon from 'enso-assets/cross.svg'
import Plus2Icon from 'enso-assets/plus2.svg'
import ReloadInCircleIcon from 'enso-assets/reload_in_circle.svg'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'
import type * as refreshHooks from '#/hooks/refreshHooks'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import KeyboardShortcutsSettingsTabBar from '#/layouts/Settings/KeyboardShortcutsSettingsTabBar'

import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
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
  const rootRef = React.useRef<HTMLDivElement>(null)
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
  const navigator2D = navigator2DProvider.useNavigator2D()

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, { length: visibleBindings.length })

  const selectedBindings =
    keyboardSelectedIndex == null ? null : visibleBindings[keyboardSelectedIndex]?.[1].bindings
  const [bindingKeyboardSelectedIndex, setBindingKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, {
      axis: keyboardNavigationHooks.Axis.horizontal,
      length: (selectedBindings?.length ?? 0) + 2,
    })

  React.useEffect(() => {
    setBindingKeyboardSelectedIndex(selectedBindings?.length ?? 0)
  }, [keyboardSelectedIndex, selectedBindings?.length, setBindingKeyboardSelectedIndex])

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      return navigator2D.register(root, {
        focusPrimaryChild: setKeyboardSelectedIndex.bind(null, 0),
      })
    }
  }, [navigator2D, setKeyboardSelectedIndex])

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
    <div ref={rootRef} className="overflow-auto px-px">
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
          {visibleBindings.map((kv, i) => {
            const [action, info] = kv
            const isNewButtonKeyboardSelected =
              keyboardSelectedIndex === i &&
              selectedBindings != null &&
              bindingKeyboardSelectedIndex === selectedBindings.length
            const isResetButtonKeyboardSelected =
              keyboardSelectedIndex === i &&
              selectedBindings != null &&
              bindingKeyboardSelectedIndex === selectedBindings.length + 1
            return (
              <tr key={action}>
                <td className="flex h-row items-center rounded-l-full bg-clip-padding pl-cell-x pr-icon-column-r">
                  <SvgMask src={info.icon ?? BlankIcon} color={info.color} className="size-icon" />
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
                        <button
                          ref={element => {
                            if (keyboardSelectedIndex === i && bindingKeyboardSelectedIndex === j) {
                              element?.focus()
                            }
                          }}
                          className={`flex rounded-full transition-colors hover:bg-hover-bg focus:bg-hover-bg ${keyboardSelectedIndex === i ? 'visible' : 'invisible group-hover:visible'} ${keyboardSelectedIndex === i && bindingKeyboardSelectedIndex === j ? 'focus-ring' : ''}`}
                          onClick={() => {
                            inputBindings.delete(action, binding)
                            doRefresh()
                          }}
                        >
                          <SvgMask src={CrossIcon} className="size-icon" />
                        </button>
                      </div>
                    ))}
                    <div className="gap-keyboard-shortcuts-buttons flex shrink-0">
                      <button
                        ref={element => {
                          if (isNewButtonKeyboardSelected) {
                            element?.focus()
                          }
                        }}
                        className={`my-auto flex rounded-full ${keyboardSelectedIndex === i ? 'visible' : 'invisible group-hover:visible'} ${
                          isNewButtonKeyboardSelected ? 'focus-ring' : ''
                        }`}
                        onClick={event => {
                          event.stopPropagation()
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
                      </button>
                      <button
                        ref={element => {
                          if (isResetButtonKeyboardSelected) {
                            element?.focus()
                          }
                        }}
                        className={`my-auto flex rounded-full ${keyboardSelectedIndex === i ? 'visible' : 'invisible group-hover:visible'} ${
                          isResetButtonKeyboardSelected ? 'focus-ring' : ''
                        }`}
                        onClick={() => {
                          inputBindings.reset(action)
                          doRefresh()
                        }}
                      >
                        <img className="size-plus-icon" src={ReloadInCircleIcon} />
                      </button>
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
  )
}
