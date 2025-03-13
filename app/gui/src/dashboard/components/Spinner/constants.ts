/** @file Constants for `Spinner`. */

/** The state of the spinner. It should go from `initial`, to `loading`, to `done`. */
export type SpinnerState = 'done' | 'initial' | 'loading-fast' | 'loading-medium' | 'loading-slow'

export const SPINNER_CSS_CLASSES: Readonly<Record<SpinnerState, string>> = {
  initial: 'dasharray-5 ease-linear',
  /* eslint-disable-next-line @typescript-eslint/naming-convention */
  'loading-slow': 'dasharray-75 duration-spinner-slow ease-linear',
  /* eslint-disable-next-line @typescript-eslint/naming-convention */
  'loading-medium': 'dasharray-75 duration-spinner-medium ease-linear',
  /* eslint-disable-next-line @typescript-eslint/naming-convention */
  'loading-fast': 'dasharray-75 duration-spinner-fast ease-linear',
  done: 'dasharray-100 duration-spinner-fast ease-in',
}
