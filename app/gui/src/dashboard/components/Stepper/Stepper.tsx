/**
 * @file
 *
 * A stepper component is used to indicate progress through a multi-step process.
 */
import * as React from 'react'

import { AnimatePresence, motion } from 'framer-motion'

import * as eventCallback from '#/hooks/eventCallbackHooks'

import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'

import { tv } from '#/utilities/tailwindVariants'
import { Step } from './Step'
import { StepContent } from './StepContent'
import * as stepperProvider from './StepperProvider'
import type { BaseRenderProps, RenderChildrenProps, RenderStepProps } from './types'
import * as stepperState from './useStepperState'

/** Props for {@link Stepper} component. */
export interface StepperProps {
  readonly state: stepperState.StepperState
  readonly children: React.ReactNode | ((props: RenderChildrenProps) => React.ReactNode)
  readonly className?:
    | string
    | ((props: BaseRenderProps) => string | null | undefined)
    | null
    | undefined
  readonly renderStep: (props: RenderStepProps) => React.ReactNode
  readonly style?:
    | React.CSSProperties
    | ((props: BaseRenderProps) => React.CSSProperties | undefined)
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

const ANIMATION_OFFSET = 15

/** A stepper component is used to indicate progress through a multi-step process. */
export function Stepper(props: StepperProps) {
  const { renderStep, children, state } = props

  const { onStepChange, currentStep, totalSteps, nextStep, previousStep, direction } = state

  const goToStep = eventCallback.useEventCallback((step: number) => {
    if (step < 0 || step >= totalSteps) {
      return
    } else {
      onStepChange(step)
      return
    }
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
      <stepperProvider.StepperProvider
        value={{ totalSteps, currentStep, goToStep, nextStep, previousStep, state }}
      >
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

            return (
              <div key={index} className={styles.step({})}>
                {renderStep(renderStepProps)}
              </div>
            )
          })}
        </div>

        <div className={styles.content()}>
          <AnimatePresence initial={false} mode="sync" custom={direction}>
            <motion.div
              key={currentStep}
              initial="enter"
              animate="center"
              exit="exit"
              variants={{
                enter: {
                  x: direction === 'back' ? -ANIMATION_OFFSET : ANIMATION_OFFSET,
                  opacity: 0,
                  position: 'absolute',
                  height: 'auto',
                  top: 0,
                  width: '100%',
                },
                center: {
                  zIndex: 1,
                  x: 0,
                  opacity: 1,
                  height: 'auto',
                  position: 'static',
                  width: '100%',
                },
                exit: (currentDirection: stepperState.StepperState['direction']) => ({
                  zIndex: 0,
                  x: currentDirection === 'back' ? ANIMATION_OFFSET : -ANIMATION_OFFSET,
                  opacity: 0,
                  position: 'absolute',
                  top: 0,
                  width: '100%',
                  height: 'auto',
                }),
              }}
              transition={{
                // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                x: { type: 'spring', stiffness: 500, damping: 50, duration: 0.2 },
                // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                opacity: { duration: 0.2 },
              }}
            >
              <ErrorBoundary>
                <Suspense loaderProps={{ minHeight: 'h32' }}>{renderChildren()}</Suspense>
              </ErrorBoundary>
            </motion.div>
          </AnimatePresence>
        </div>
      </stepperProvider.StepperProvider>
    </div>
  )
}

Stepper.Step = Step
Stepper.StepContent = StepContent
Stepper.useStepperState = stepperState.useStepperState
