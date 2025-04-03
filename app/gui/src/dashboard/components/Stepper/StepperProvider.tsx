/**
 * @file
 *
 * StepperProvider component
 */
import * as React from 'react'

import invariant from 'tiny-invariant'

import type { StepperState } from './useStepperState'

/** StepperProvider props */
export interface StepperContextType {
  readonly currentStep: number
  readonly goToStep: (step: number) => void
  readonly totalSteps: number
  readonly nextStep: () => void
  readonly previousStep: () => void
  readonly state: StepperState
}

const StepperContext = React.createContext<StepperContextType | null>(null)

/**
 * Hook to use the stepper context
 * @internal
 */
export function useStepperContext() {
  const context = React.useContext(StepperContext)

  invariant(context, 'useStepper must be used within a StepperProvider')

  return context
}

// eslint-disable-next-line no-restricted-syntax
export const StepperProvider = StepperContext.Provider
