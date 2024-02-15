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
  const scrollContainerRef = React.useRef<HTMLDivElement>(null)
  const bodyRef = React.useRef<HTMLTableSectionElement>(null)

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
    <div className="flex flex-col flex-1 gap-2.5 w-full pr-4">
      <h3 className="font-bold text-xl h-9.5 py-0.5">Keyboard shortcuts</h3>
      {/* There is a horizontal scrollbar for some reason without `px-px`. */}
      <div ref={scrollContainerRef} className="overflow-auto px-px">
        <table className="rounded-rows table-fixed border-collapse">
          <thead className="sticky top-0">
            <tr className="text-left text-sm font-semibold h-8">
              <th className="min-w-4 pl-2 pr-1.5">{/* Icon */}</th>
              <th className="min-w-36 px-2">Name</th>
              <th className="px-2">Shortcuts</th>
              <th className="px-2 w-full">Description</th>
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
                    <td className="flex h-8 items-center rounded-l-full pl-2 pr-1.5 bg-clip-padding">
                      <SvgMask
                        src={info.icon ?? BlankIcon}
                        color={info.color}
                        className="w-4 h-4"
                      />
                    </td>
                    <td className="px-2 bg-clip-padding border-transparent border-l-2 border-r-2">
                      {info.name}
                    </td>
                    <td className="group min-w-max pl-2 bg-clip-padding border-transparent border-l-2 border-r-2">
                      <div className="flex gap-2 pr-4">
                        {info.bindings.map((binding, i) => (
                          <div key={i} className="inline-flex items-center gap-1 shrink-0">
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
                        <div className="flex gap-1 shrink-0">
                          <button
                            className="align-middle invisible group-hover:visible"
                            onClick={() => {
                              // FIXME: capture keyboard shortcut
                              inputBindings.add(action, '')
                              doRefresh()
                            }}
                          >
                            <img className="w-4.5 h-4.5" src={Plus2Icon} />
                          </button>
                          <button
                            className="align-middle invisible group-hover:visible"
                            onClick={() => {
                              inputBindings.reset(action)
                              doRefresh()
                            }}
                          >
                            <img className="w-4.5 h-4.5" src={Plus2Icon} />
                          </button>
                        </div>
                      </div>
                    </td>
                    <td className="rounded-r-full px-2 bg-clip-padding border-transparent border-l-2 border-r-2">
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
