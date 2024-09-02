/**
 * @file
 *
 * A stepper component is used to indicate progress through a multi-step process.
 */
import * as React from 'react'

import { AnimatePresence, motion } from 'framer-motion'
import * as tvw from 'tailwind-variants'

import DoneIcon from '#/assets/check_mark.svg'

import * as eventCallback from '#/hooks/eventCallbackHooks'

import * as ariaComponents from '#/components/AriaComponents'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import SvgMask from '#/components/SvgMask'

import * as stepperProvider from './StepperProvider'
import * as stepperState from './useStepperState'

/**
 * Render props for the stepper component.
 */
export interface BaseRenderProps {
  readonly goToStep: (step: number) => void
  readonly nextStep: () => void
  readonly previousStep: () => void
  readonly currentStep: number
  readonly totalSteps: number
}

/**
 * Render props for rendering children of the stepper component.
 */
export interface RenderChildrenProps extends BaseRenderProps {
  readonly isFirst: boolean
  readonly isLast: boolean
}

/**
 * Render props for lazy rendering of steps.
 */
export interface RenderStepProps extends BaseRenderProps {
  /**
   * The index of the step, starting from 0.
   */
  readonly index: number
  readonly isCurrent: boolean
  readonly isCompleted: boolean
  readonly isFirst: boolean
  readonly isLast: boolean
  readonly isDisabled: boolean
}

/**
 * Render props for styling the stepper component.
 */
export interface RenderStepperProps {
  readonly currentStep: number
  readonly totalSteps: number
  readonly isFirst: boolean
  readonly isLast: boolean
}

/**
 * Props for {@link Stepper} component.
 */
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

const STEPPER_STYLES = tvw.tv({
  base: 'flex flex-col items-center w-full gap-4',
  slots: {
    steps: 'flex items-center justify-between w-full',
    step: 'flex-1 last:flex-none',
    content: 'relative w-full',
  },
})

const ANIMATION_OFFSET = 15

/**
 * A stepper component is used to indicate progress through a multi-step process.
 */
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

  const classes = STEPPER_STYLES({})

  const style = typeof props.style === 'function' ? props.style(baseRenderProps) : props.style

  /**
   * Render children of the stepper component.
   */
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
      className={classes.base({
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
        <div className={classes.steps()}>
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
              <div key={index} className={classes.step({})}>
                {renderStep(renderStepProps)}
              </div>
            )
          })}
        </div>

        <div className={classes.content()}>
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

/** A prop with the given type, or a function to produce a value of the given type. */
type StepProp<T> = T | ((props: RenderStepProps) => T)

/**
 * Props for {@link Step} component.
 */
export interface StepProps extends RenderStepProps {
  readonly className?: StepProp<string | null | undefined>
  readonly icon?: StepProp<React.ReactElement | string | null | undefined>
  readonly completeIcon?: StepProp<React.ReactElement | string | null | undefined>
  readonly title?: StepProp<React.ReactElement | string | null | undefined>
  readonly description?: StepProp<React.ReactElement | string | null | undefined>
  readonly children?: StepProp<React.ReactNode>
}

const STEP_STYLES = tvw.tv({
  base: 'relative flex items-center gap-2 select-none',
  slots: {
    icon: 'w-6 h-6 border-0.5 flex-none border-current rounded-full flex items-center justify-center transition-colors duration-200',
    titleContainer: '-mt-1 flex flex-col items-start justify-start transition-colors duration-200',
    content: 'flex-1',
  },
  variants: {
    position: { first: 'rounded-l-full', last: 'rounded-r-full' },
    status: {
      completed: {
        base: 'text-primary',
        icon: 'bg-primary border-transparent text-invert',
        content: 'text-primary',
      },
      current: { base: 'text-primary', content: 'text-primary/30' },
      next: { base: 'text-primary/30', content: 'text-primary/30' },
    },
  },
})

/**
 * A step component is used to represent a single step in a stepper component.
 */
function Step(props: StepProps) {
  const {
    index,
    title,
    description,
    isCompleted,
    goToStep,
    nextStep,
    previousStep,
    totalSteps,
    currentStep,
    isCurrent,
    isLast,
    isFirst,
    isDisabled,
    className,
    children,
    icon = (
      <ariaComponents.Text variant="subtitle" color="current" aria-hidden>
        {index + 1}
      </ariaComponents.Text>
    ),
    completeIcon = DoneIcon,
  } = props

  const { state } = stepperProvider.useStepperContext()

  const renderStepProps = {
    isCompleted,
    goToStep,
    nextStep,
    previousStep,
    totalSteps,
    currentStep,
    isCurrent,
    isLast,
    isFirst,
    isDisabled,
    index,
  } satisfies RenderStepProps

  const classes = typeof className === 'function' ? className(renderStepProps) : className
  const descriptionElement =
    typeof description === 'function' ? description(renderStepProps) : description
  const titleElement = typeof title === 'function' ? title(renderStepProps) : title
  const iconElement = typeof icon === 'function' ? icon(renderStepProps) : icon
  const doneIconElement =
    typeof completeIcon === 'function' ? completeIcon(renderStepProps) : completeIcon

  const styles = STEP_STYLES({
    className: classes,
    position:
      isFirst ? 'first'
      : isLast ? 'last'
      : undefined,
    status:
      isCompleted ? 'completed'
      : isCurrent ? 'current'
      : 'next',
  })

  const stepAnimationRotation = 30
  const stepAnimationScale = 0.5

  return (
    <div className={styles.base()}>
      <AnimatePresence initial={false} mode="sync" custom={state.direction}>
        <motion.div
          key={isCompleted ? 'done' : 'icon'}
          className={styles.icon()}
          initial="enter"
          animate="center"
          exit="exit"
          variants={{
            enter: {
              rotate:
                state.direction === 'forward' ? -stepAnimationRotation : stepAnimationRotation,
              scale: stepAnimationScale,
              opacity: 0,
              position: 'absolute',
              top: 0,
            },
            center: {
              rotate: 0,
              scale: 1,
              opacity: 1,
              position: 'static',
            },
            exit: (direction: stepperState.StepperState['direction']) => ({
              rotate: direction === 'back' ? -stepAnimationRotation : stepAnimationRotation,
              scale: stepAnimationScale,
              opacity: 0,
              position: 'absolute',
              top: 0,
            }),
          }}
          transition={{
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            rotate: { type: 'spring', stiffness: 500, damping: 100, bounce: 0, duration: 0.2 },
          }}
        >
          {(() => {
            const renderIconElement = isCompleted ? doneIconElement : iconElement

            if (renderIconElement == null) {
              return null
            } else if (typeof renderIconElement === 'string') {
              return <SvgMask src={renderIconElement} />
            } else {
              return renderIconElement
            }
          })()}
        </motion.div>
      </AnimatePresence>

      <div className={styles.titleContainer()}>
        {titleElement != null && (
          <div>
            {typeof titleElement === 'string' ?
              <ariaComponents.Text nowrap color="current">
                {titleElement}
              </ariaComponents.Text>
            : titleElement}
          </div>
        )}

        {descriptionElement != null && (
          <div>
            {typeof descriptionElement === 'string' ?
              <ariaComponents.Text variant="body" color="current" truncate="2">
                {descriptionElement}
              </ariaComponents.Text>
            : descriptionElement}
          </div>
        )}
      </div>
      <div className={styles.content()}>
        {typeof children === 'function' ? children(renderStepProps) : children}
      </div>
    </div>
  )
}

Stepper.Step = Step
Stepper.useStepperState = stepperState.useStepperState
