/** @file Base modal component that provides the full-screen element that blocks mouse events. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'

import FocusRoot from '#/components/styled/FocusRoot'

import { ClearPressResponder } from '#/components/aria'
import * as tailwindVariants from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const MODAL_VARIANTS = tailwindVariants.tv({
  base: 'inset z-1',
  variants: {
    centered: { true: 'size-screen fixed grid place-items-center' },
  },
})

// =================
// === Component ===
// =================

/** Props for a {@link Modal}. */
export interface ModalProps
  extends Readonly<React.PropsWithChildren>,
    Readonly<tailwindVariants.VariantProps<typeof MODAL_VARIANTS>> {
  /** If `true`, disables `data-testid` because it will not be visible. */
  readonly hidden?: boolean
  readonly centered?: boolean | undefined
  readonly style?: React.CSSProperties
  readonly className?: string
  readonly onClick?: React.MouseEventHandler<HTMLDivElement>
  readonly onContextMenu?: React.MouseEventHandler<HTMLDivElement>
}

/**
 * A fullscreen modal with content at the center. The background is fully opaque by default;
 * background transparency can be enabled with Tailwind's `bg-opacity` classes, like
 * `className="bg-opacity-50"`.
 */
export default function Modal(props: ModalProps) {
  const { hidden = false, children, style, onClick, onContextMenu, ...variantProps } = props
  const { unsetModal } = modalProvider.useSetModal()

  return (
    // Required so that `Button`s and `Checkbox`es contained inside do not trigger any
    // ancestor `DialogTrigger`s.
    <ClearPressResponder>
      <FocusRoot active={!hidden}>
        {(innerProps) => (
          <div
            {...(!hidden ? { 'data-testid': 'modal-background' } : {})}
            style={style}
            className={MODAL_VARIANTS(variantProps)}
            onClick={
              onClick ??
              ((event) => {
                if (event.currentTarget === event.target && getSelection()?.type !== 'Range') {
                  event.stopPropagation()
                  unsetModal()
                }
              })
            }
            onContextMenu={onContextMenu}
            {...innerProps}
            onKeyDown={(event) => {
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
    </ClearPressResponder>
  )
}
