/**
 * @file
 *
 * Types for the stepper component.
 */

/** Render props for the stepper component. */
export interface BaseRenderProps {
  readonly goToStep: (step: number) => void
  readonly nextStep: () => void
  readonly previousStep: () => void
  readonly currentStep: number
  readonly totalSteps: number
}

/** Render props for rendering children of the stepper component. */
export interface RenderChildrenProps extends BaseRenderProps {
  readonly isFirst: boolean
  readonly isLast: boolean
}

/** Render props for lazy rendering of steps. */
export interface RenderStepProps extends BaseRenderProps {
  /** The index of the step, starting from 0. */
  readonly index: number
  readonly isCurrent: boolean
  readonly isCompleted: boolean
  readonly isFirst: boolean
  readonly isLast: boolean
  readonly isDisabled: boolean
}
