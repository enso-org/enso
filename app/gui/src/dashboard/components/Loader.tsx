/** @file A full-screen loading spinner. */
import { StatelessSpinner, type SpinnerState } from '#/components/StatelessSpinner'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { memo } from 'react'

const STYLES = tv({
  base: 'animate-appear-delayed flex h-full w-full items-center justify-center duration-200',
  variants: {
    minHeight: {
      full: 'h-full',
      h6: 'min-h-6',
      h8: 'min-h-8',
      h10: 'min-h-10',
      h12: 'min-h-12',
      h16: 'min-h-16',
      h20: 'min-h-20',
      h24: 'min-h-24',
      h32: 'min-h-32',
      h40: 'min-h-40',
      h48: 'min-h-48',
      h56: 'min-h-56',
      h64: 'min-h-64',
      screen: 'min-h-screen',
      custom: '',
    },
    height: {
      full: 'h-full',
      screen: 'h-screen',
      custom: '',
    },
    color: {
      primary: 'text-primary/50',
    },
  },
})

const SIZE_MAP: Record<Size, number> = {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  large: 64,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  medium: 32,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  small: 16,
}

/** The possible sizes for a {@link Loader}. */
export type Size = 'large' | 'medium' | 'small'

/** Props for a {@link Loader}. */
export interface LoaderProps extends VariantProps<typeof STYLES> {
  readonly children?: React.ReactNode
  readonly className?: string
  readonly size?: Size | number
  readonly state?: SpinnerState
}

/** A full-screen loading spinner. */

export const Loader = memo(function Loader(props: LoaderProps) {
  const {
    children,
    className,
    size: sizeRaw = 'medium',
    state = 'loading-fast',
    minHeight = 'full',
    color = 'primary',
    height = 'full',
  } = props

  const size = typeof sizeRaw === 'number' ? sizeRaw : SIZE_MAP[sizeRaw]

  return (
    <div className={STYLES({ minHeight, className, color, height })}>
      <div className="flex flex-col items-center gap-2">
        <StatelessSpinner size={size} phase={state} className="text-current" />
        {children}
      </div>
    </div>
  )
})
