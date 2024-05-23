/** @file Base modal component that provides the full-screen element that blocks mouse events. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import FocusRoot from '#/components/styled/FocusRoot'

// =================
// === Component ===
// =================

/** Props for a {@link Modal}. */
export interface ModalProps extends Readonly<React.PropsWithChildren> {
  /** If `true`, disables `data-testid` because it will not be visible. */
  readonly hidden?: boolean
  // This can intentionally be `undefined`, in order to simplify consumers of this component.
  // eslint-disable-next-line no-restricted-syntax
  readonly centered?: boolean | undefined
  readonly style?: React.CSSProperties
  readonly className?: string
  readonly onClick?: React.MouseEventHandler<HTMLDivElement>
  readonly onContextMenu?: React.MouseEventHandler<HTMLDivElement>
}

/** A fullscreen modal with content at the center. The background is fully opaque by default;
 * background transparency can be enabled with Tailwind's `bg-opacity` classes, like
 * `className="bg-opacity-50"`. */
export default function Modal(props: ModalProps) {
  const { hidden = false, children, centered = false, style, className } = props
  const { onClick, onContextMenu } = props
  const { unsetModal } = modalProvider.useSetModal()

  return (
    <FocusRoot active={!hidden}>
      {innerProps => (
        <div
          {...(!hidden ? { 'data-testid': 'modal-background' } : {})}
          style={style}
          className={`inset z-1 ${centered ? 'size-screen fixed grid place-items-center' : ''} ${
            className ?? ''
          }`}
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
          {...innerProps}
          onKeyDown={event => {
            innerProps.onKeyDown?.(event)
            if (event.key !== 'Escape') {
              event.stopPropagation()
            }
          }}
        >
          {children}
        </div>
      )}
    </FocusRoot>
  )
}
