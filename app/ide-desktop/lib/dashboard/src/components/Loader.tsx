/**
 * @file
 * A full-screen loading spinner.
 */
import * as twv from 'tailwind-variants'

import Spinner, * as spinnerModule from '#/components/Spinner'

/**
 * Props for a {@link Loader}.
 */
export interface LoaderProps extends twv.VariantProps<typeof STYLES> {
  readonly className?: string
  readonly size?: Sizes | number
  readonly state?: spinnerModule.SpinnerState
}

/**
 * The possible sizes for a {@link Loader}.
 */
export type Sizes = 'large' | 'medium' | 'small'

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

const SIZE_MAP: Record<Sizes, number> = {
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  large: 64,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  medium: 32,
  // eslint-disable-next-line @typescript-eslint/no-magic-numbers
  small: 16,
}

/**
 * A full-screen loading spinner.
 */
export function Loader(props: LoaderProps) {
  const {
    className,
    size = 'medium',
    state = spinnerModule.SpinnerState.loadingFast,
    minHeight = 'full',
    color = 'primary',
  } = props

  const sizeValue = typeof size === 'number' ? size : SIZE_MAP[size]

  return (
    <div className={STYLES({ minHeight, className, color })}>
      <Spinner size={sizeValue} state={state} className="text-current" />
    </div>
  )
}
