/** @file An area that contains focusable children. */
import * as React from 'react'

import * as detect from 'enso-common/src/detect'

import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import * as aria from '#/components/aria'

// =================
// === FocusArea ===
// =================

/** Props for a {@link Menu} */
export interface MenuProps<T extends object> extends Readonly<aria.MenuProps<T>> {
  readonly active?: boolean
  readonly currentKey: aria.Key | null
  readonly firstKey: aria.Key
  readonly lastKey: aria.Key
}

/** The inner component of {@link Menu}. This is required to use the
 * {@link aria.useFocusManager} hook. */
export default function Menu<T extends object>(props: MenuProps<T>) {
  const { active = true, currentKey, firstKey, lastKey, children, ...menuProps } = props
  const navigator2D = navigator2DProvider.useNavigator2D()
  const rootRef = React.useRef<HTMLDivElement | null>(null)
  const cleanupRef = React.useRef(() => {})
  const onSelectionChangeRef = React.useRef(props.onSelectionChange)
  onSelectionChangeRef.current = props.onSelectionChange
  const currentKeyRef = React.useRef(currentKey)
  currentKeyRef.current = currentKey
  const firstKeyRef = React.useRef(firstKey)
  firstKeyRef.current = firstKey
  const lastKeyRef = React.useRef(lastKey)
  lastKeyRef.current = lastKey

  return (
    <aria.Menu
      ref={element => {
        rootRef.current = element
        cleanupRef.current()
        if (active && element != null) {
          let lastFocusedElement = document.activeElement
          const onKeyDown = () => {
            lastFocusedElement = document.activeElement
          }
          element.addEventListener('keydown', onKeyDown)
          const unregisterNavigator = navigator2D.register(element, {
            allowNavigation: () => {
              return document.activeElement === lastFocusedElement
            },
            focusPrimaryChild: () => {
              onSelectionChangeRef.current?.(
                new Set([currentKeyRef.current ?? firstKeyRef.current])
              )
            },
            focusWhenPressed: {
              down: () => {
                onSelectionChangeRef.current?.(new Set([firstKeyRef.current]))
              },
              up: () => {
                onSelectionChangeRef.current?.(new Set([lastKeyRef.current]))
              },
            },
          })
          cleanupRef.current = () => {
            unregisterNavigator()
            element.removeEventListener('keydown', onKeyDown)
          }
        } else {
          cleanupRef.current = () => {}
        }
        if (element != null && detect.IS_DEV_MODE) {
          if (active) {
            element.dataset.focusArea = ''
          } else {
            delete element.dataset.focusArea
          }
        }
      }}
      shouldFocusWrap={false}
      {...menuProps}
    >
      {children}
    </aria.Menu>
  )
}
