/** @file Settings tab for editing keyboard shortcuts. */
import * as React from 'react'

import BlankIcon from 'enso-assets/blank.svg'
import CrossIcon from 'enso-assets/cross.svg'
import Plus2Icon from 'enso-assets/plus2.svg'
import ReloadInCircleIcon from 'enso-assets/reload_in_circle.svg'

import type * as inputBindingsModule from '#/configurations/inputBindings'

import * as refreshHooks from '#/hooks/refreshHooks'

import * as inputBindingsManager from '#/providers/InputBindingsProvider'
import * as modalProvider from '#/providers/ModalProvider'

import KeyboardShortcut from '#/components/dashboard/KeyboardShortcut'
import SvgMask from '#/components/SvgMask'

import CaptureKeyboardShortcutModal from '#/modals/CaptureKeyboardShortcutModal'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'

import * as object from '#/utilities/object'

// ====================================
// === KeyboardShortcutsSettingsTab ===
// ====================================

/** Settings tab for viewing and editing account information. */
export default function KeyboardShortcutsSettingsTab() {
  const inputBindings = inputBindingsManager.useInputBindings()
  const { setModal } = modalProvider.useSetModal()
  const [refresh, doRefresh] = refreshHooks.useRefresh()
  const scrollContainerRef = React.useRef<HTMLDivElement>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)
  const allShortcuts = React.useMemo(() => {
    // This is REQUIRED, in order to avoid disabling the `react-hooks/exhaustive-deps` lint.
    // eslint-disable-next-line @typescript-eslint/no-unused-expressions
    refresh
    return new Set(Object.values(inputBindings.metadata).flatMap(value => value.bindings))
  }, [inputBindings.metadata, refresh])

  // This is required to prevent the table body from overlapping the table header, because
  // the table header is transparent.
  React.useEffect(() => {
    const body = bodyRef.current
    const scrollContainer = scrollContainerRef.current
    if (body != null && scrollContainer != null) {
      let isClipPathUpdateQueued = false
      const updateClipPath = () => {
        isClipPathUpdateQueued = false
        body.style.clipPath = `inset(${scrollContainer.scrollTop}px 0 0 0)`
      }
      const onScroll = () => {
        if (!isClipPathUpdateQueued) {
          isClipPathUpdateQueued = true
          requestAnimationFrame(updateClipPath)
        }
      }
      updateClipPath()
      scrollContainer.addEventListener('scroll', onScroll)
      return () => {
        scrollContainer.removeEventListener('scroll', onScroll)
      }
    } else {
      return
    }
  }, [/* should never change */ scrollContainerRef])

  return (
    <div className="flex w-full flex-1 flex-col gap-settings-section-header">
      <h3 className="settings-subheading">Keyboard shortcuts</h3>
      <div className="flex gap-drive-bar">
        <button
          className="flex h-row items-center rounded-full bg-frame px-new-project-button-x"
          onClick={event => {
            event.stopPropagation()
            setModal(
              <ConfirmDeleteModal
                actionText="reset all keyboard shortcuts"
                actionButtonLabel="Reset All"
                doDelete={() => {
                  for (const k in inputBindings.metadata) {
                    // eslint-disable-next-line no-restricted-syntax
                    inputBindings.reset(k as inputBindingsModule.DashboardBindingKey)
                  }
                  doRefresh()
                }}
              />
            )
          }}
        >
          <span className="text whitespace-nowrap font-semibold">Reset All</span>
        </button>
      </div>
      {/* There is a horizontal scrollbar for some reason without `px-px`. */}
      {/* eslint-disable-next-line no-restricted-syntax */}
      <div ref={scrollContainerRef} className="overflow-auto px-px">
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
            {object
              .unsafeEntries(inputBindings.metadata)
              .filter(kv => kv[1].rebindable !== false)
              .map(kv => {
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
                        {info.bindings.map((binding, i) => (
                          <div
                            key={i}
                            className="inline-flex shrink-0 items-center gap-keyboard-shortcuts-button"
                          >
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
                        <div className="gap-keyboard-shortcuts-buttons flex shrink-0">
                          <button
                            className="invisible align-middle group-hover:visible"
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
                            className="invisible align-middle group-hover:visible"
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
    </div>
  )
}
