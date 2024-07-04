/** @file A full-screen loading spinner. */
import StatelessSpinner, * as spinnerModule from '#/components/StatelessSpinner'

import * as twv from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const STYLES = twv.tv({
  base: 'animate-appear-delayed flex h-full w-full items-center justify-center duration-200',
  variants: {
    minHeight: {
      full: 'h-full',
      h6: 'h-6',
      h8: 'h-8',
      h10: 'h-10',
      h12: 'h-12',
      h16: 'h-16',
      h20: 'h-20',
      h24: 'h-24',
      h32: 'h-32',
      h40: 'h-40',
      h48: 'h-48',
      h56: 'h-56',
      h64: 'h-64',
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

// ============
// === Size ===
// ============

/** The possible sizes for a {@link Loader}. */
export type Size = 'large' | 'medium' | 'small'

// ==============
// === Loader ===
// ==============

/** Props for a {@link Loader}. */
export interface LoaderProps extends twv.VariantProps<typeof STYLES> {
  readonly className?: string
  readonly size?: Size | number
  readonly state?: spinnerModule.SpinnerState
}

/** A full-screen loading spinner. */
export function Loader(props: LoaderProps) {
  const {
    className,
    size: sizeRaw = 'medium',
    state = spinnerModule.SpinnerState.loadingFast,
    minHeight = 'full',
    color = 'primary',
  } = props

  const size = typeof sizeRaw === 'number' ? sizeRaw : SIZE_MAP[sizeRaw]

  return (
    <div className={STYLES({ minHeight, className, color })}>
      <StatelessSpinner size={size} state={state} className="text-current" />
    </div>
  )
}
