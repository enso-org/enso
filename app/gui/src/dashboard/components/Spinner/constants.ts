/** @file Constants for `Spinner`. */

/** The state of the spinner. It should go from `initial`, to `loading`, to `done`. */
export type SpinnerPhase = 'done' | 'initial' | 'loading-fast' | 'loading-medium' | 'loading-slow'

export const SPINNER_CSS_CLASSES: Readonly<Record<SpinnerPhase, string>> = {
  /* eslint-disable @typescript-eslint/naming-convention */
  initial: 'dasharray-5 ease-linear',
  'loading-slow': 'dasharray-75 duration-spinner-slow ease-linear',
  'loading-medium': 'dasharray-75 duration-spinner-medium ease-linear',
  'loading-fast': 'dasharray-75 duration-spinner-fast ease-linear',
  done: 'dasharray-100 duration-spinner-fast ease-in',
  /* eslint-enable @typescript-eslint/naming-convention */
}
