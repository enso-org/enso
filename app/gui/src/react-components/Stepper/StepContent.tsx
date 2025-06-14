/**
 * @file
 * Component to render the step content.
 */
import type { ReactElement, ReactNode } from 'react'
import { useStepperContext } from './StepperProvider'
import type { RenderChildrenProps } from './types'

/** Props for {@link StepContent} component. */
export interface StepContentProps {
  readonly index: number
  readonly children: ReactNode | ((props: RenderChildrenProps) => ReactNode)
  readonly forceRender?: boolean
}

/** Step content component. Renders the step content if the step is current or if `forceRender` is true. */
export function StepContent(props: StepContentProps): ReactElement | null {
  const { index, children, forceRender = false } = props
  const { currentStep, goToStep, nextStep, previousStep, totalSteps } = useStepperContext()

  const isCurrent = currentStep === index

  const renderProps = {
    currentStep,
    totalSteps,
    isFirst: currentStep === 0,
    isLast: currentStep === totalSteps - 1,
    goToStep,
    nextStep,
    previousStep,
  } satisfies RenderChildrenProps

  if (isCurrent || forceRender) {
    return <>{typeof children === 'function' ? children(renderProps) : children}</>
  } else {
    return null
  }
}
