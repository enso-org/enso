/** @file Base modal component that provides the full-screen element that blocks mouse events. */
import { ClearPressResponder } from '#/components/aria'
import { unsetModal } from '#/providers/ModalProvider'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import type { CSSProperties, MouseEventHandler, PropsWithChildren } from 'react'

const MODAL_VARIANTS = tv({
  base: 'inset z-1',
  variants: {
    centered: { true: 'size-screen fixed grid place-items-center' },
  },
})

/** Props for a {@link Modal}. */
export interface ModalProps
  extends Readonly<PropsWithChildren>,
    Readonly<VariantProps<typeof MODAL_VARIANTS>> {
  /** If `true`, disables `data-testid` because it will not be visible. */
  readonly hidden?: boolean
  readonly centered?: boolean | undefined
  readonly style?: CSSProperties
  readonly className?: string
  readonly onClick?: MouseEventHandler<HTMLDivElement>
  readonly onContextMenu?: MouseEventHandler<HTMLDivElement>
}

/**
 * A fullscreen modal with content at the center. The background is fully opaque by default;
 * background transparency can be enabled with Tailwind's `bg-opacity` classes, like
 * `className="bg-opacity-50"`.
 */
export function Modal(props: ModalProps) {
  const { hidden = false, children, style, onClick, onContextMenu, ...variantProps } = props

  return (
    // Required so that `Button`s and `Checkbox`es contained inside do not trigger any
    // ancestor `DialogTrigger`s.
    <ClearPressResponder>
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
        onKeyDown={(event) => {
          if (event.key !== 'Escape') {
            event.stopPropagation()
          }
        }}
      >
        {children}
      </div>
    </ClearPressResponder>
  )
}
