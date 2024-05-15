/**
 * @file
 * A full-screen loading spinner.
 */
import * as tw from 'tailwind-merge'

import Spinner, * as spinnerModule from '#/components/Spinner'

/**
 * Props for a {@link Loader}.
 */
export interface LoaderProps {
  readonly className?: string
  readonly size?: number
}

const DEFAULT_SIZE = 32

/**
 * A full-screen loading spinner.
 */
export function Loader(props: LoaderProps) {
  const { className, size = DEFAULT_SIZE } = props
  return (
    <div className={tw.twMerge('flex h-full w-full items-center justify-center', className)}>
      <Spinner size={size} state={spinnerModule.SpinnerState.loadingMedium} />
    </div>
  )
}
