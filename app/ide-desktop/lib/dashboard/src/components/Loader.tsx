/** @file A full-screen loading spinner. */
import Spinner, * as spinnerModule from '#/components/Spinner'

import * as twv from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const STYLES = twv.tv({
  base: 'flex flex-none h-full w-full items-center justify-center',
  variants: {
    minHeight: {
      full: 'h-full',
      h4: 'h-4',
      h5: 'h-5',
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
      primary: 'text-primary',
      inverted: 'text-white',
    },
    appearDelayed: { true: 'animate-appear-delayed duration-200' },
  },
  defaultVariants: {
    appearDelayed: true,
    color: 'primary',
    minHeight: 'full',
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
    size: sizeRaw = 'medium',
    state = spinnerModule.SpinnerState.loadingFast,
    className,
    minHeight,
    color,
    appearDelayed,
  } = props

  const size = typeof sizeRaw === 'number' ? sizeRaw : SIZE_MAP[sizeRaw]

  return (
    <div className={STYLES({ minHeight, className, color, appearDelayed })}>
      <Spinner size={size} state={state} className="text-current" />
    </div>
  )
}
