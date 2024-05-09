/** @file Hooks related to context menus. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import ContextMenu from '#/components/ContextMenu'
import ContextMenus from '#/components/ContextMenus'

// ======================
// === contextMenuRef ===
// ======================

/** Return a ref that attaches a context menu event listener.
 * Should be used ONLY if the element does not expose an `onContextMenu` prop. */
export function useContextMenuRef(
  key: string,
  label: string,
  createEntries: (position: Pick<React.MouseEvent, 'pageX' | 'pageY'>) => JSX.Element | null
) {
  const { setModal } = modalProvider.useSetModal()
  const createEntriesRef = React.useRef(createEntries)
  createEntriesRef.current = createEntries
  const cleanupRef = React.useRef(() => {})
  const [contextMenuRef] = React.useState(() => (element: HTMLElement | null) => {
    cleanupRef.current()
    if (element == null) {
      cleanupRef.current = () => {}
    } else {
      const onContextMenu = (event: MouseEvent) => {
        const position = { pageX: event.pageX, pageY: event.pageY }
        const children = createEntriesRef.current(position)
        if (children != null) {
          event.preventDefault()
          event.stopPropagation()
          setModal(
            <ContextMenus
              ref={contextMenusElement => {
                if (contextMenusElement != null) {
                  const rect = contextMenusElement.getBoundingClientRect()
                  position.pageX = rect.left
                  position.pageY = rect.top
                }
              }}
              key={key}
              event={event}
            >
              <ContextMenu aria-label={label}>{children}</ContextMenu>
            </ContextMenus>
          )
        }
      }
      element.addEventListener('contextmenu', onContextMenu)
      cleanupRef.current = () => {
        element.removeEventListener('contextmenu', onContextMenu)
      }
    }
  })
  return contextMenuRef
}
