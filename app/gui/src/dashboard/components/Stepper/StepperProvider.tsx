/** @file A provider for Stepper context. */
import { createContext, useContext } from 'react'
import invariant from 'tiny-invariant'

/** Props for {@link StepperProvider}. */
export interface StepperContextType {
  readonly currentStep: number
  readonly goToStep: (step: number) => void
  readonly totalSteps: number
  readonly nextStep: () => void
  readonly previousStep: () => void
}

const StepperContext = createContext<StepperContextType | null>(null)

/**
 * Hook to use the stepper context
 * @internal
 */
// eslint-disable-next-line react-refresh/only-export-components
export function useStepperContext() {
  const context = useContext(StepperContext)
  invariant(context, 'useStepper must be used within a StepperProvider')
  return context
}

// eslint-disable-next-line no-restricted-syntax
export const StepperProvider = StepperContext.Provider
