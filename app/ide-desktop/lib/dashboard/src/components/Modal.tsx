/** @file Base modal component that provides the full-screen element that blocks mouse events. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

// =================
// === Component ===
// =================

/** Props for a {@link Modal}. */
export interface ModalProps extends React.PropsWithChildren {
  /** If `true`, disables `data-testid` because it will not be visible. */
  hidden?: boolean
  // This can intentionally be `undefined`, in order to simplify consumers of this component.
  // eslint-disable-next-line no-restricted-syntax
  centered?: boolean | undefined
  style?: React.CSSProperties
  className?: string
  onClick?: React.MouseEventHandler<HTMLDivElement>
  onContextMenu?: React.MouseEventHandler<HTMLDivElement>
}

/** A fullscreen modal with content at the center. The background is fully opaque by default;
 * background transparency can be enabled with Tailwind's `bg-opacity` classes, like
 * `className="bg-opacity-50"`. */
export default function Modal(props: ModalProps) {
  const { hidden = false, children, centered = false, style, className } = props
  const { onClick, onContextMenu } = props
  const { unsetModal } = modalProvider.useSetModal()

  return (
    <div
      // The name comes from a third-party API and cannot be changed.
      // eslint-disable-next-line @typescript-eslint/naming-convention
      {...(!hidden ? { 'data-testid': 'modal-background' } : {})}
      style={style}
      // This MUST be z-3, unlike all other elements, because it MUST show above the IDE.
      className={`inset-0 z-3 ${
        centered ? 'fixed w-screen h-screen grid place-items-center' : ''
      } ${className ?? ''}`}
      onClick={
        onClick ??
        (event => {
          if (event.currentTarget === event.target && getSelection()?.type !== 'Range') {
            event.stopPropagation()
            unsetModal()
          }
        })
      }
      onContextMenu={onContextMenu}
    >
      {children}
    </div>
  )
}
