/** @file Hooks for `Stepper`. */
import { StepperContext } from '#/components/Stepper/constants'
import { useContext } from 'react'
import invariant from 'tiny-invariant'

/**
 * Hook to use the stepper context
 * @internal
 */
export function useStepperContext() {
  const context = useContext(StepperContext)
  invariant(context, 'useStepper must be used within a StepperProvider')
  return context
}
