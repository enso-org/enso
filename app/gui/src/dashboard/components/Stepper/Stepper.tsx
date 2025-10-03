/** @file UI for a multi-step process. */
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { tv } from '#/utilities/tailwindVariants'
import type { CSSProperties, ReactNode } from 'react'
import { Step } from './Step'
import { StepContent } from './StepContent'
import { StepperProvider } from './StepperProvider'
import type { BaseRenderProps, RenderChildrenProps, RenderStepProps } from './types'
import { useStepperState, type StepperState } from './useStepperState'

/** Props for {@link Stepper} component. */
export interface StepperProps {
  readonly state: StepperState
  readonly children: ReactNode | ((props: RenderChildrenProps) => ReactNode)
  readonly className?:
    | string
    | ((props: BaseRenderProps) => string | null | undefined)
    | null
    | undefined
  readonly renderStep?: ((props: RenderStepProps) => ReactNode) | null
  readonly style?:
    | CSSProperties
    | ((props: BaseRenderProps) => CSSProperties | undefined)
    | undefined
}

const STEPPER_STYLES = tv({
  base: 'flex flex-col items-center w-full gap-4',
  slots: {
    steps: 'flex items-center justify-between w-full',
    step: 'flex-1 last:flex-none',
    content: 'relative w-full',
  },
})

/** A stepper component is used to indicate progress through a multi-step process. */
export function Stepper(props: StepperProps) {
  const { renderStep, children, state } = props

  const { onStepChange, currentStep, totalSteps, nextStep, previousStep } = state

  const goToStep = useEventCallback((step: number) => {
    if (step < 0 || step >= totalSteps) return
    onStepChange(step)
  })

  const baseRenderProps = {
    goToStep,
    nextStep,
    previousStep,
    currentStep,
    totalSteps,
  } satisfies BaseRenderProps

  const styles = STEPPER_STYLES({})

  const style = typeof props.style === 'function' ? props.style(baseRenderProps) : props.style

  /** Render children of the stepper component. */
  const renderChildren = () => {
    const renderProps = {
      currentStep,
      totalSteps,
      isFirst: currentStep === 0,
      isLast: currentStep === totalSteps - 1,
      goToStep,
      nextStep,
      previousStep,
    } satisfies RenderChildrenProps

    return typeof children === 'function' ? children(renderProps) : children
  }

  return (
    <div
      className={styles.base({
        className:
          typeof props.className === 'function' ?
            props.className(baseRenderProps)
          : props.className,
      })}
      style={style}
    >
      <StepperProvider value={{ totalSteps, currentStep, goToStep, nextStep, previousStep }}>
        {renderStep == null ? null : (
          <div className={styles.steps()}>
            {Array.from({ length: totalSteps }).map((_, index) => {
              const renderStepProps = {
                index,
                currentStep,
                totalSteps,
                isFirst: index === 0,
                isLast: index === totalSteps - 1,
                nextStep,
                previousStep,
                goToStep,
                isCompleted: index < currentStep,
                isCurrent: index === currentStep,
                isDisabled: index > currentStep,
              } satisfies RenderStepProps

              const nextRenderStep = renderStep(renderStepProps)

              if (nextRenderStep == null) {
                return null
              }

              return (
                <div key={index} className={styles.step({})}>
                  {nextRenderStep}
                </div>
              )
            })}
          </div>
        )}

        <div className={styles.content()}>
          <div key={currentStep}>
            <ErrorBoundary>
              <Suspense loaderProps={{ minHeight: 'h32' }}>{renderChildren()}</Suspense>
            </ErrorBoundary>
          </div>
        </div>
      </StepperProvider>
    </div>
  )
}

Stepper.Step = Step
Stepper.StepContent = StepContent
Stepper.useStepperState = useStepperState
