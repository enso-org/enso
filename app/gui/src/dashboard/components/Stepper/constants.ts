/** @file StepperProvider component. */
import { createContext } from 'react'
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

export const StepperContext = createContext<StepperContextType | null>(null)
